package viper.carbon.proofgen

import isabelle.ast.TermIdent


case class RelationalLemmaData(relationalLemmaName: String, relationalLemmaAssmDefName: String)

trait RelationalProofData {

  def theoryName: String

  def relationalLemmaName: String

  def relationalLemmaAssumptionDefName: String

  // The first translation record contains the initial label_hm_translation, with no labels
  def translationRecord0Def: TermIdent

  // The second translation record contains the label_hm_translation after the old state has been initialized,
  // with the default old label set to point to the correct boogie variables for the old mask and heap
  def translationRecord1Def: TermIdent

  def varRelationListDef: TermIdent

  def varRelationBoundsLemma: String

  def basicDisjointnessLemma: String

  def varContextVprDef: TermIdent

  def varContextBplDef: TermIdent

}
