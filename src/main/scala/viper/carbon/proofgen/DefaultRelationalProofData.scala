package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultRelationalProofData(override val theoryName: String, varRelationBoundsLemmaName: String, varContextBplDefName: String) extends RelationalProofData {

  def qualifyName(name: String) = IsaUtil.qualifyName(theoryName, name)

  override val varRelationBoundsLemma : String = qualifyName(varRelationBoundsLemmaName)

  override val varContextBplDef : TermIdent = TermIdent(qualifyName(varContextBplDefName))

}
