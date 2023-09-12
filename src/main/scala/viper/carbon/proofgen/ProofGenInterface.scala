package viper.carbon.proofgen

import viper.carbon.boogie.{Decl, Procedure}
import viper.carbon.proofgen.hints.{BoogieDeclProofHint, MethodProofHint, StmtProofHint}
import viper.carbon.verifier.Environment
import viper.silver.{ast => sil}

import java.nio.file.Path

trait ProofGenInterface {

  def boogieProofDir : Path

  def generateProofForMethod(m: sil.Method, procBpl: Procedure, procBplEnv: Environment, methodProofHint: MethodProofHint) : Unit

  def generateProofForPreamble(preamble: Seq[Decl]) : Unit

  def finishProof() : Unit

}
