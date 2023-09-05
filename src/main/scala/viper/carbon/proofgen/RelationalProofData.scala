package viper.carbon.proofgen

import isabelle.ast.TermIdent

trait RelationalProofData {

  def theoryName: String

  def varRelationBoundsLemma: String

  def varContextBplDef: TermIdent

}
