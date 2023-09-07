package viper.carbon.proofgen

import isabelle.ast.TermIdent

trait RelationalProofData {

  def theoryName: String

  def translationRecordDef: TermIdent

  def varRelationListDef: TermIdent

  def varRelationBoundsLemma: String

  def basicDisjointnessLemma: String

  def varContextBplDef: TermIdent

}
