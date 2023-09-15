package viper.carbon.proofgen

import isabelle.ast.IsaUtil

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import viper.silver.{ast => sil}
import viper.carbon.boogie.{Decl, Procedure}
import viper.carbon.modules.{HeapModule, PermModule}
import viper.carbon.proofgen.end_to_end.{DefaultIsaViperEndToEndGlobalData, EndToEndGlobalDataHelper, IsaViperEndToEndGlobalData, MethodEndToEndProofGenerator}
import viper.carbon.proofgen.functions.FunctionProofGenInterface
import viper.carbon.proofgen.hints.{BoogieDeclProofHint, MethodProofHint, StmtProofHint}
import viper.carbon.proofgen.util.FileUtil
import viper.carbon.verifier.Environment

//a directory with path proofGenDir must exist

case class CarbonModules(
                        heapModule: HeapModule,
                        permModule: PermModule
                        )
class DefaultProofGenInterface(val proofDir: Path,
                               val vprProg: sil.Program,
                               val carbonModules: CarbonModules) extends ProofGenInterface {

  val funProofGenInterface = new FunctionProofGenInterface(carbonModules.heapModule, carbonModules.permModule)

  val boogieProofDirName : String = "boogie_proofs"
  val boogieProofDir : Path = proofDir.resolve(boogieProofDirName)

  val globalDataTheoryName = "global_data_vpr"

  val globalDataBpl : IsaBoogieGlobalAccessor = IsaBoogieGlobalAccessor("global_data", vprProg.fields)

  val vprProgGlobalData : IsaViperGlobalDataAccessor =
    {

      //we assume that proofs accessing global data are always one level deeper (that's why the path "../")
      val (theory, globalDataAccessor, helperTheories) = IsaVprProgramGenerator.globalData(vprProg, globalDataBpl, globalDataTheoryName, "../")
      for (thy <- theory +: helperTheories) {
        StoreTheory.storeTheory(thy, proofDir)
      }
      globalDataAccessor
    }

  val endToEndGlobalData: DefaultIsaViperEndToEndGlobalData  = EndToEndGlobalDataHelper.generateEndToEndData("global_data_end_to_end")

  /***
    * Creates and stores the Isabelle proof relating the Viper and Boogie programs
    *
    * Should only be invoked if Boogie has already been run and produced a proof at [[boogieProofDir()]]
    */
  def finishProof() : Unit = {
    if(!Files.exists(boogieProofDir)) {
      sys.error(s"Cannot generate CPG proof: Boogie proofs are not stored at ${boogieProofDir.toAbsolutePath.toString}")
    }

    /* We do not need the ROOT files produced by Boogie. Moreover, it is useful to make sure that all produced ROOT files
       are only for Viper->Boogie proofs, because then a test framework for Viper->Boogie proofs can just iterate over
       all ROOT files in a directory without any further filters.
     */
    FileUtil.deleteFilesInDirectory(boogieProofDir, filename => filename.equals("ROOT"))

    generateRootFile()
  }


  def generateRootFile() : Unit = {
    import viper.carbon.proofgen.util.StringBuilderExtension._

    val sb = new StringBuilder("session " + IsaUtil.convertToValidIsabelleIdentifier(proofDir.getFileName.toString) + " = TotalViper +").newLine

    sb.append("directories").newLine

    sb.append(boogieProofDirName).newLine

    FileUtil.listOfSubdirectories(boogieProofDir) match {
      case Some(boogieSubDirs) =>
        boogieSubDirs.foreach(subDir => sb.appendInner(boogieProofDirName+"/"+subDir).newLine)
      case None => sys.error("Could not list subdirectories in Boogie proof folder")
    }

    vprProg.methods.foreach(
      method => sb.append(methodProofDirName(method)).newLine
    )

    sb.append("theories").newLine

    sb.append(globalDataTheoryName).newLine

    vprProg.methods.foreach(
      method => {
        val methodDir = methodProofDirName(method)
        for (methodTheoryName <- Seq(methodProgTheory(method), methodRelationalProofName(method))) {
          sb.appendInner(methodDir+"/"+methodTheoryName).newLine
        }
      }
    )

    Files.write(proofDir.resolve("ROOT"), sb.toString().getBytes(StandardCharsets.UTF_8))
  }

  def methodRelationalProofName(m: sil.Method) : String = s"relational_proof_${m.name}"

  def methodEndToEndProofName(m: sil.Method) : String = s"end_to_end_proof_${m.name}"

  def methodProofDirName(m: sil.Method) : String =  s"method_proof_${m.name}"
  def methodProofPath(m: sil.Method) : Path =
    proofDir.resolve(methodProofDirName(m))

  def methodProgTheory(m: sil.Method) : String = m.name+"_vpr_prog"

  def generateProofForMethod(m: sil.Method, procBpl: Procedure, procBplEnv: Environment, methodProofHint: MethodProofHint) : Unit = {
      val dir: Path = Files.createDirectory(methodProofPath(m))

      /* Viper program representation */
      val varTranslation = DeBruijnTranslation.freshTranslation((m.formalArgs ++ m.formalReturns) map (varDecl => varDecl.localVar))

      /* Viper <-> Boogie proof */
      //here we are assuming how Boogie proof generation names proof folders
      //TODO: does not work if there are name clashes between namespaces --> pretty printer might not use given name
      val bplProgTheoryPath =
        s"../${boogieProofDirName}/${procBpl.name.name}_proofs/${procBpl.name.name}_before_ast_to_cfg_prog"

      val bplProcAccessor = new IsaBoogieProcAccessor(
        procBpl,
        procBplEnv,
        globalDataBpl,
        bplProgTheoryPath)

      val relationalProofGenerator = MethodRelationalProofGenerator(
        methodRelationalProofName(m),
        vprProgGlobalData.allMethodsAccessor.methodAccessor(m.name),
        vprProgGlobalData,
        varTranslation,
        bplProcAccessor,
        methodProofHint,
        funProofGenInterface)

      val (relationalProofTheory, relationalProofData) = relationalProofGenerator.generateRelationalProof()

      StoreTheory.storeTheory(relationalProofTheory, dir)

      val endToEndProofGenerator = MethodEndToEndProofGenerator(
        theoryName = methodEndToEndProofName(m),
        methodAccessor = vprProgGlobalData.allMethodsAccessor.methodAccessor(m.name),
        viperProgAccessor = vprProgGlobalData,
        endToEndData = endToEndGlobalData,
        relationalProofData = relationalProofData,
        boogieProgAccessor = bplProcAccessor
      )

      val (endToEndTheoryProof, methodEndToEndProofData) = endToEndProofGenerator.generatePartialEndToEndProof()

      StoreTheory.storeTheory(endToEndTheoryProof, dir)
  }

  override def generateProofForPreamble(preamble: Seq[Decl]): Unit = {
    val declHints = preamble.map(BoogieDeclProofHint.extractHintsFromAllDecls).flatten

    val theory = EndToEndGlobalDataHelper.generateEndToEndTheory(endToEndGlobalData, vprProgGlobalData, globalDataBpl, declHints, boogieProofDirName)
    StoreTheory.storeTheory(theory, proofDir)
  }

}
