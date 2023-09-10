package viper.carbon.proofgen.end_to_end

import isabelle.ast.TermIdent

trait IsaViperEndToEndGlobalData {

  def theoryName : String

  def ctxtVpr : TermIdent

  def programTotalProgEqLemma : String

  def constantsLookupWithoutGlobalsLemma : String

  def constantsLookupWithGlobalsLemma : String


  def funInterpVprBpl : TermIdent

  def funInterpVprBplWfLemma : String

  def funInterpBplWfLemma : String

  def ranFieldRelLemma : String

  def fieldPropLemma : String

  def axiomSatLemma : String

}
