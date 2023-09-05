package viper.carbon.proofgen.end_to_end

import isabelle.ast.TermIdent

trait IsaViperEndToEndGlobalData {

  def theoryName : String

  def ctxtVpr : TermIdent

  def funInterpVprBpl : TermIdent

  def funInterpVprBplWfLemma : String

  def funInterpBplWfLemma : String

  def fieldPropLemma : String

  def axiomSatLemma : String

}
