package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultRelationalProofData(override val theoryName: String, relationalLemmaData: RelationalLemmaData, translationRecordDefName: String, varRelationListDefName: String, varRelationBoundsLemmaName: String, basicDisjointnessLemmaName: String, varContextVprDefName: String, varContextBplDefName: String)
  extends RelationalProofData {

  def qualifyName(name: String) : String = IsaUtil.qualifyName(theoryName, name)

  override def relationalLemmaName : String = qualifyName(relationalLemmaData.relationalLemmaName)

  override def relationalLemmaAssumptionDefName: String = qualifyName(relationalLemmaData.relationalLemmaAssmDefName)

  override val translationRecordDef: TermIdent = TermIdent(qualifyName(translationRecordDefName))

  override def varRelationListDef: TermIdent = TermIdent(qualifyName(varRelationListDefName))

  override val varRelationBoundsLemma : String = qualifyName(varRelationBoundsLemmaName)

  override def basicDisjointnessLemma: String = qualifyName(basicDisjointnessLemmaName)

  override val varContextVprDef : TermIdent = TermIdent(qualifyName(varContextVprDefName))

  override val varContextBplDef : TermIdent = TermIdent(qualifyName(varContextBplDefName))

}
