// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon

import ch.qos.logback.classic.Logger
import viper.silver.frontend.{SilFrontend, SilFrontendConfig}
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.{Reporter, StdIOReporter}
import viper.silver.verifier.{Verifier => SilVerifier}

/**
 * The main object for Carbon containing the execution start-point.
 */
object Carbon extends CarbonFrontend(StdIOReporter("carbon_reporter"), ViperStdOutLogger("Carbon", "INFO").get) {
  def main(args: Array[String]): Unit = {
    execute(args)
    specifyAppExitCode()
    sys.exit(appExitCode)
  }
}

class CarbonFrontend(override val reporter: Reporter,
                     override val logger: Logger) extends SilFrontend {

  private var carbonInstance: CarbonVerifier = _

  override def backendTypeFormat: Option[String] = Some("Boogie")

  def createVerifier(fullCmd: String) = {
    carbonInstance = CarbonVerifier(reporter, Seq("Arguments: " -> fullCmd))

    carbonInstance
  }

  def configureVerifier(args: Seq[String]) = {
  	carbonInstance.parseCommandLine(args)

    carbonInstance.config
  }

  override def init(verifier: SilVerifier): Unit = {
    verifier match {
      case carbon: CarbonVerifier =>
        carbonInstance = carbon
      case _ =>
        sys.error( "Expected verifier to be an instance of CarbonVerifier but got an instance " +
                  s"of ${verifier.getClass}")
    }

    super.init(verifier)

    _config = carbonInstance.config
  }
}

class CarbonConfig(args: Seq[String]) extends SilFrontendConfig(args, "Carbon") {
  val boogieProverLog = opt[String]("proverLog",
    descr = "Prover log file written by Boogie (default: none)",
    default = None,
    noshort = true
  )

  val boogieOut = opt[String]("print",
    descr = "Write the Boogie output file to the provided filename (default: none)",
    default = None,
    noshort = true
  )

  val boogieOpt = opt[String]("boogieOpt",
  descr = "Option(s) to pass-through as options to Boogie (changing the output generated by Boogie is not supported) (default: none)",
  default = None,
  noshort = true
  )

  val boogieExecutable = opt[String]("boogieExe",
    descr = "Manually-specified full path to Boogie.exe executable (default: ${BOOGIE_EXE})",
    default = None,
    noshort = true
  )

  val Z3executable = opt[String]("z3Exe",
    descr = "Manually-specified full path to Z3.exe executable (default: ${Z3_EXE})",
    default = None,
    noshort = true
  )

  val disableAllocEncoding = opt[Boolean]("disableAllocEncoding",
    descr = "Disable Allocation-related assumptions (default: enabled)",
    default = None,
    noshort = true
  )

  val desugarPolymorphicMaps = opt[Boolean]("desugarPolymorphicMaps",
    descr = "Do not use polymorphic maps in the Boogie encoding and instead desugar them (default: false).",
    default = Some(false),
    noshort = true
  )

  val genProofs = opt[Boolean]("genProofs",
    descr = "Generate proofs (default: false).",
    default = Some(false),
    noshort = true
  )

  val onlyCheckProofGenSupport = opt[Int]("onlyCheckProofGenSupport",
    descr = "Only check whether proof generation is supported for program. That is, the program is not verified and proofs " +
      "are not produced. Settings are if this should be disabled (0), enabled and check if proof generation supports the program (1)," +
      "enabled and check if program can be rewritten in a systematic way such that proof generation supports the program (2). (default: 0)",
    default = Some(0),
    noshort = true
  )

  validateOpt(genProofs, desugarPolymorphicMaps, disableAllocEncoding) {
    case (Some(true), desugarPolyMapOption, disableAllocOption) if !desugarPolyMapOption.getOrElse(false) || !disableAllocOption.getOrElse(false) =>
      Left(s"Option ${genProofs.name} option requires options ${desugarPolymorphicMaps.name} and ${disableAllocEncoding.name}")
    case _ => Right()
  }

  validateOpt(onlyCheckProofGenSupport) {
    case (Some(i)) if i < 0 || i > 2 =>
      Left(s"Option ${onlyCheckProofGenSupport.name} option must be 0,1,or 2")
    case _ => Right()
  }

  verify()
}
