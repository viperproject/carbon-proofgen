package viper.carbon.proofgen

import isabelle.ast.TermIdent


case class RelationalLemmaData(relationalLemmaName: String, relationalLemmaAssmDefName: String)

trait RelationalProofData {

  def theoryName: String

  def relationalLemmaName: String

  def relationalLemmaAssumptionDefName: String

  def translationRecordDef: TermIdent

  def varRelationListDef: TermIdent

  def varRelationBoundsLemma: String

  def basicDisjointnessLemma: String

  def varContextVprDef: TermIdent

  def varContextBplDef: TermIdent

}
