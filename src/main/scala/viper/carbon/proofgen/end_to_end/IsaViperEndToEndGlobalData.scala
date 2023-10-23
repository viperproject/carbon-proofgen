package viper.carbon.proofgen.end_to_end

import isabelle.ast.{TermIdent, TypeIsa}

trait IsaViperEndToEndGlobalData {

  def theoryName : String

  def ctxtVpr : TermIdent

  def abstractValueType: TypeIsa

  def programTotalProgEqLemma : String

  def constantsLookupWithoutGlobalsLemma : String

  def constantsLookupWithGlobalsLemma : String


  def funInterpVprBpl : TermIdent

  def funInterpVprBplWfLemma : String

  def funInterpBplWfLemma : String

  def ranFieldRelLemma : String

  def injFieldRelLemma : String

  def fieldPropWithoutGlobalsLemma : String

  def fieldPropWithGlobalsLemma : String

  def declaredFieldsFieldRelDomEqLemma : String

  def axiomSatLemma : String

}
