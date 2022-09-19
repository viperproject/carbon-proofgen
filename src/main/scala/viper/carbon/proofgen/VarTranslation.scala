package viper.carbon.proofgen

import isabelle.ast.Term

trait VarTranslation[T] {

  def addFreshVariables(x: Seq[T]) : VarTranslation[T]

  def translateVariable(x: T) : Option[Term]

  def translateVariableId(x: T) : Option[Int]

}
