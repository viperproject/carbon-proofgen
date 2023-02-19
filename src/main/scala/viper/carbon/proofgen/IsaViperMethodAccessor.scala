package viper.carbon.proofgen

import isabelle.ast.{Term, TermIdent}
import viper.silver.{ast => sil}

trait IsaViperGlobalDataAccessor {

  val theoryName: String
  def vprProgram() : TermIdent
  def fields() : TermIdent
  def fieldIdent(f: sil.Field) : Term
  def fieldLookupLemma(fieldName: String) : String

}

trait IsaViperMethodAccessor {

  val theoryName: String
  def methodBody() : TermIdent
  def methodArgs() : TermIdent
}

case class IsaViperProgAccessor(globalDataAccessor: IsaViperGlobalDataAccessor, methodAccessor: IsaViperMethodAccessor)
