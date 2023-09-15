package viper.carbon.proofgen

import isabelle.ast.Term

trait VarTranslation[T] {

  /**
    *
    * @param xs
    * @return same variable translation where variables in [[xs]] are added
    *         The effect of the call vartr.[[addFreshVariables]](Seq(x0,x1,...,xn)) is the same as
    *         (...(vartr.[[addFreshVariable(x0)]]).[[addFreshVariable(x1)]]...).[[addFreshVariable(xn)]]
    */
  def addFreshVariables(xs: Seq[T]) : VarTranslation[T]

  def addFreshVariable(x: T) : VarTranslation[T] = addFreshVariables(Seq(x))

  def translateVariable(x: T) : Option[Term]

  def translateVariableId(x: T) : Option[Int]

  def availableVariables() : Seq[T]

}
