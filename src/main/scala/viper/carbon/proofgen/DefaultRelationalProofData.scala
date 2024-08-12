package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultRelationalProofData(override val theoryName: String, relationalLemmaData: RelationalLemmaData, translationRecord0DefName: String, translationRecord1DefName: String, varRelationListDefName: String, varRelationBoundsLemmaName: String, basicDisjointnessLemmaName: String, varContextVprDefName: String, varContextBplDefName: String)
  extends RelationalProofData {

  def qualifyName(name: String) : String = IsaUtil.qualifyName(theoryName, name)

  override def relationalLemmaName : String = qualifyName(relationalLemmaData.relationalLemmaName)

  override def relationalLemmaAssumptionDefName: String = qualifyName(relationalLemmaData.relationalLemmaAssmDefName)

  override val translationRecord0Def: TermIdent = TermIdent(qualifyName(translationRecord0DefName))

  override val translationRecord1Def: TermIdent = TermIdent(qualifyName(translationRecord1DefName))

  override def varRelationListDef: TermIdent = TermIdent(qualifyName(varRelationListDefName))

  override val varRelationBoundsLemma : String = qualifyName(varRelationBoundsLemmaName)

  override def basicDisjointnessLemma: String = qualifyName(basicDisjointnessLemmaName)

  override val varContextVprDef : TermIdent = TermIdent(qualifyName(varContextVprDefName))

  override val varContextBplDef : TermIdent = TermIdent(qualifyName(varContextBplDefName))

}
