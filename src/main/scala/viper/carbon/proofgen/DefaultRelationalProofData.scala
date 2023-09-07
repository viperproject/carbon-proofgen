package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultRelationalProofData(override val theoryName: String, translationRecordDefName: String, varRelationListDefName: String, varRelationBoundsLemmaName: String, basicDisjointnessLemmaName: String, varContextBplDefName: String)
  extends RelationalProofData {

  def qualifyName(name: String) = IsaUtil.qualifyName(theoryName, name)

  override val translationRecordDef: TermIdent = TermIdent(qualifyName(translationRecordDefName))

  override def varRelationListDef: TermIdent = TermIdent(qualifyName(varRelationListDefName))

  override val varRelationBoundsLemma : String = qualifyName(varRelationBoundsLemmaName)

  override def basicDisjointnessLemma: String = qualifyName(basicDisjointnessLemmaName)

  override val varContextBplDef : TermIdent = TermIdent(qualifyName(varContextBplDefName))

}
