package viper.carbon.proofgen

import viper.carbon.boogie.Procedure
import viper.carbon.proofgen.hints.StmtProofHint
import viper.carbon.verifier.Environment
import viper.silver.{ast => sil}

import java.nio.file.Path

trait ProofGenInterface {

  def boogieProofDir : Path
  def generateProofForMethod(m: sil.Method, procBpl: Procedure, procBplEnv: Environment, bodyProofHint: StmtProofHint)
  def finishProof() : Unit

}
