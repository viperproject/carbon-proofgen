package viper.carbon.proofgen

import isabelle.ast.{Term, TermIdent}
import viper.silver.{ast => sil}

trait IsaViperGlobalDataAccessor {

  def theoryName : String
  def vprProgram : TermIdent
  def fields : TermIdent
  def fieldRel : TermIdent
  def fieldIdent(f: sil.Field) : Term
  def fieldLookupLemma(fieldName: String) : String

}

trait IsaViperMethodAccessor {

  def theoryName: String
  def globalDataAccessor : IsaViperGlobalDataAccessor
  def methodBody : TermIdent
  def methodArgs : TermIdent

}