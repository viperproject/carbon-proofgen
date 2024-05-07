// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon

import boogie.{BoogieModelTransformer, Namespace}
import modules.impls._
import viper.silver.{ast => sil}
import viper.silver.ast.{MagicWand, Program, Quasihavoc, Quasihavocall}
import viper.silver.utility.Paths
import viper.silver.verifier._
import verifier.{BoogieDependency, BoogieInterface, Verifier}
import viper.carbon.proofgen.{CarbonModules, DefaultProofGenInterface, ProofGenInterface}
import viper.silver.ast.Program
import viper.silver.ast.utility.QuantifiedPermissions.SourceQuantifiedPermissionAssertion
import viper.silver.ast.utility.Visitor

import java.io.{BufferedOutputStream, File, FileOutputStream, IOException}
import viper.silver.frontend.{MissingDependencyException, NativeModel, VariablesModel}
import viper.silver.reporter.Reporter

import java.nio.file.Files
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
 * The main class to perform verification of Viper programs.  Deals with command-line arguments, configuration
 * of modules and choosing which module implementations to use.
 *
 * Debug information can either be set using the constructor argument or the setter.
 */
case class CarbonVerifier(override val reporter: Reporter,
                          private var _debugInfo: Seq[(String, Any)] = Nil) extends Verifier with viper.silver.verifier.Verifier with BoogieInterface {

  var env = null

  private var _config: CarbonConfig = _
  def config = _config

  def start(): Unit = {}
  def stop(): Unit = {
    if (allModules != null) {
      allModules foreach (m => {
        m.stop()
      })
    }
    stopBoogie()
  }

  private var namespaceId = 0
  override def freshNamespace(name: String): Namespace = {
    namespaceId += 1
    Namespace(name, namespaceId)
  }

  val stmtModule = new DefaultStmtModule(this)
  val expModule = new DefaultExpModule(this)
  val typeModule = new DefaultTypeModule(this)
  val exhaleModule = new DefaultExhaleModule(this)
  val inhaleModule = new DefaultInhaleModule(this)
  val heapModule = new DefaultHeapModule(this)
  val funcPredModule = new DefaultFuncPredModule(this)
  val permModule = new QuantifiedPermModule(this)
  val mainModule = new DefaultMainModule(this)
  val stateModule = new DefaultStateModule(this)
  val domainModule = new DefaultDomainModule(this)
  val seqModule = new DefaultSeqModule(this)
  val setModule = new DefaultSetModule(this)
  val mapModule = new DefaultMapModule(this)
  val wandModule = new DefaultWandModule(this)
  val loopModule = new DefaultLoopModule(this)

  // initialize all modules
  allModules foreach (m => {
    m.start()
  })

  /** The default location for Boogie (the environment variable ${BOOGIE_EXE}). */
  lazy val boogieDefault: String = new File(Paths.resolveEnvVars("${BOOGIE_EXE}")).getAbsolutePath

  /** The default location for Z3 (the environment variable ${Z3_EXE}). */
  lazy val z3Default: String = new File(Paths.resolveEnvVars("${Z3_EXE}")).getAbsolutePath

  /** The (resolved) path where Boogie is supposed to be located. */

  def boogiePath = if (config != null) config.boogieExecutable.toOption match {
    case Some(path) => new File(path).getAbsolutePath
    case None => boogieDefault
  } else boogieDefault

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path = if (config != null) config.Z3executable.toOption match {
    case Some(path) => {new File(path).getAbsolutePath}
    case None => z3Default
  } else z3Default

  def assumeInjectivityOnInhale = if (config != null) config.assumeInjectivityOnInhale.toOption match {
    case Some(b) => b
    case None => false
  }
  else false

  override def usePolyMapsInEncoding =
    if (config != null) {
      config.desugarPolymorphicMaps.toOption match {
        case Some(b) => !b
        case None => true
      }
    } else {
      true
    }

  override def generateProofs : Boolean =
    if (config != null) {
      config.genProofs.getOrElse(false)
    } else {
      false
    }

  def onlyCheckProofGenSupport : ProofGenSubsetCheckSetting =
    if(config != null) {
      val setting = config.onlyCheckProofGenSupport.getOrElse(0)
      if(setting == 0) {
        CheckAndGenerateProofs
      } else if(setting == 1) {
        OnlyCheckSubset(true)
      } else if(setting == 2) {
        OnlyCheckSubset(false)
      }  else {
        CheckAndGenerateProofs
      }
    } else {
      CheckAndGenerateProofs
    }

  def name: String = "carbon"
  def version: String = "1.0"
  def buildVersion = version
  def copyright: String = "(c) 2013 ETH Zurich"

  def getDebugInfo = _debugInfo
  def debugInfo(info: Seq[(String, Any)]): Unit = {
    _debugInfo = info
  }

  def toolDesc = s"$name $version"
  def dependencyDescs = {
    (dependencies map (dep => {
      s"${dep.name} ${dep.version}, located at ${dep.location}."
    }))
  }

  def parseCommandLine(options: Seq[String]): Unit = {
    _config = new CarbonConfig(options)
  }

  lazy val dependencies: Seq[Dependency] = {
    import scala.sys.process._
    val unknownVersion = "(?)"
    List(new BoogieDependency(boogiePath), new Dependency {
      def name = "Z3"
      def version = {
        try {
          val v = List(z3Path, "-version").lazyLines.to(List)
          if (v.size == 1 && v(0).startsWith("Z3 version ")) {
            v(0).substring("Z3 version ".size)
          } else {
            unknownVersion
          }
        }
        catch {
          case _: IOException => throw MissingDependencyException("Z3 couldn't be found.")
        }

      }
      def location = z3Path
    })
  }

  def verify(program: Program) : VerificationResult = {
    _program = program

    proofGenInit(program)

    val unsupportedFeatures : Seq[AbstractError] =
      program.shallowCollect(
        n =>
          n match {
            case q: Quasihavocall =>
              ConsistencyError("Carbon does not support quasihavocall", q.pos)
            case q@Quasihavoc(_, MagicWand(_, _)) =>
              ConsistencyError("Carbon does not support quasihavoc of magic wands", q.pos)
          }
      )

    if(unsupportedFeatures.nonEmpty) {
      return Failure(unsupportedFeatures)
    }

    // reset all modules
    allModules map (m => m.reset())
    heapModule.enableAllocationEncoding = config == null || !config.disableAllocEncoding.isSupplied // NOTE: config == null happens on the build server / via sbt test

    var transformNames = false
    if (config == null) Seq() else config.counterexample.toOption match {
      case Some(NativeModel) =>
      case Some(VariablesModel) => transformNames = true
      case None =>
      case Some(v) => sys.error("Invalid option: " + v)
    }

    val (tProg, translatedNames) = mainModule.translate(program, reporter)
    _translated = tProg


    val options = {
      if (config == null) {
        Nil
      } else {
        (config.boogieProverLog.toOption match {
      case Some(l) =>
        List("/proverLog:" + l + " ")
      case None =>
        Nil
      }) ++
        (config.boogieOpt.toOption match {
          case Some(l) =>
            l.split(" ")
          case None =>
            Nil
        }) ++
          (config.counterexample.toOption match {
            case Some(_) => {
              /* [2020-05-31 Marco] We use /mv:- instead of /printModel:1 because Boogie, at least the versions I tried,
               * does not properly separate models for different errors when it prints multiple ones and uses multiple
               * threads. I.e., it ill mix lines belonging to different models, which makes them useless.
               */
              List("/mv:-")
            }
            case _ => Nil
          }) ++
          (if(generateProofs) {
            /* TODO: adjust Boogie proof generation such that can use /noVerify flag here for Boogie so that need not
               go through the entire VC generation + querying of SMT solver if just want Isabelle representation of
               program
             */
            List("/isaProgNoProofs:1", "/useIdBasedLemmaNaming", "/proofOutputDir", proofGenInterface.boogieProofDir.toAbsolutePath.toString)
          } else {
            Nil
          })
      }
    }

    var timeout: Option[Int] = None

    if(config != null)
    {
      config.boogieOut.toOption match {
        case Some(filename) =>
          // write Boogie program to the specified file
          val f = new File(filename)
          val stream = new BufferedOutputStream(new FileOutputStream(f))
          stream.write(_translated.toString.getBytes)
          stream.close()
        case None =>
      }
      timeout = config.timeout.toOption
    }

    invokeBoogie(_translated, options, timeout) match {
      case (version,result) =>
        if (version!=null) { dependencies.foreach(_ match {
          case b:BoogieDependency => b.version = version
          case _ => }) }

        result match {
          case Failure(errors) if transformNames => {
            errors.foreach(e =>  BoogieModelTransformer.transformCounterexample(e, translatedNames))
          }
          case _ => result
        }

        if(generateProofs) {
          proofGenInterface.finishProof()
        }

        result
    }

  }

  private var _translated: viper.carbon.boogie.Program = null
  def translated = _translated

  private var _program: Program = null
  def program = _program
  def program_=(p : Program): Unit = {
    _program = p
  }

  def replaceProgram(prog : Program) = {this.program = prog}

  private var _proofGenInterface: ProofGenInterface = null
  def proofGenInterface = _proofGenInterface

  private def proofGenInit(program: Program) : Unit = {
    if(generateProofs || onlyCheckProofGenSupport.isInstanceOf[OnlyCheckSubset]) {

      val strictCheck = onlyCheckProofGenSupport match {
        case OnlyCheckSubset(b) => b
        case CheckAndGenerateProofs => true
      }

      val unsupportedNodeOpt = proofGenSupportsProgram(program, strictCheck)
      unsupportedNodeOpt match {
        case Some(unsupportedNode) =>
          println("Failure: Proof generation does not support program because of node " + unsupportedNode.toString)
          sys.exit(17)
        case None =>
      }

      if(onlyCheckProofGenSupport.isInstanceOf[OnlyCheckSubset]) {
        //early exit if only proof generation support needs to be checked
        println("Success: Proof generation supports program.")
        sys.exit(0)
      }

      val proofGenDirSuffix = org.apache.commons.io.FilenameUtils.getBaseName(config.file.toOption.get)+ "_proof"
      val proofGenDirName = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss_").format(LocalDateTime.now) + proofGenDirSuffix
      val proofGenDir = java.nio.file.Paths.get(System.getProperty("user.dir")).resolve(proofGenDirName)
      Files.createDirectory(proofGenDir)


      val carbonModules = CarbonModules(heapModule = heapModule, permModule = permModule)
      _proofGenInterface = new DefaultProofGenInterface(proofGenDir, program, carbonModules)
    }
  }

  /***
    *
    * @param program
    * @param useBasicVersion
    * @return None if proof generation supports program (or if [[useBasicVersion]] is false, then program could be rewritten to support it)
    *         and otherwise Some(n) where node n is the cause for why proof generation does not support the program.
    */
  private def proofGenSupportsProgram(program: Program, useBasicVersion: Boolean) : Option[sil.Node] =
      Visitor.find(program, sil.utility.Nodes.subnodes)(
        {
          node =>
            node match {
              /** types */
              case _: sil.SetType => node
              case _: sil.MultisetType => node
              case _: sil.MapType => node
              case _: sil.BackendType => node
              case _: sil.SeqType => node
              case _: sil.ExtensionType => node
              /** members */
              case _: sil.Predicate => node
              case _: sil.Function => node
              case _: sil.Domain => node
              /** statements (predicate and wand statements already excluded by [[sil.Predicate]] and [[sil.MagicWand]]) */
              case _: sil.NewStmt if useBasicVersion => node
              case sil.Goto(_) => node
              case _: sil.While if useBasicVersion => node
              case _: sil.ExtensionStmt => node
              case _: sil.LabelledOld if useBasicVersion => node
              case _: sil.Old if useBasicVersion => node
              case _: sil.Quasihavoc => node
              case _: sil.Quasihavocall => node
              case _: sil.Label if useBasicVersion => node
              /** assertions (predicates already excluded by [[sil.Predicate]] */
              case _: sil.ForPerm => node
              case _: sil.MagicWand => node
              case _: sil.QuantifiedExp => node
              case SourceQuantifiedPermissionAssertion(_, _) => node
              case _: sil.InhaleExhaleExp => node
              /** expressions */
              case _: sil.CurrentPerm => node
              case _: sil.ExtensionExp => node
              case _: sil.Let => node
              case _: sil.WildcardPerm => node
              case _: sil.EpsilonPerm => node //epsilon should not occur in Viper programs (old feature that has been dropped)
            }
        }
      )
}

sealed trait ProofGenSubsetCheckSetting
case object CheckAndGenerateProofs extends ProofGenSubsetCheckSetting
case class OnlyCheckSubset(strictCheck: Boolean) extends ProofGenSubsetCheckSetting
