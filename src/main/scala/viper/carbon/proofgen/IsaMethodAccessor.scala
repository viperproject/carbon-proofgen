package viper.carbon.proofgen

import isabelle.ast.TermIdent

import java.nio.file.Path

trait IsaMethodAccessor {

  val methodTheoryPath : String
  def methodBody() : TermIdent
  def methodArgs() : TermIdent
}
