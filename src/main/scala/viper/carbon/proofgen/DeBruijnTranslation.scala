package viper.carbon.proofgen
import isabelle.ast.Term

class DeBruijnTranslation[T](val debruijnMapping: Map[T, Int]) extends VarTranslation[T] {

  override def addFreshVariables(xs: Seq[T]): VarTranslation[T] = {
    val res = DeBruijnTranslation.updateMapWithFreshVars(debruijnMapping, xs)

    new DeBruijnTranslation(res)
  }

  override def translateVariable(x: T): Option[Term] = translateVariableId(x).map(id => ViperIsaTerm.varExpr(id))

  override def translateVariableId(x: T): Option[Int] = debruijnMapping.get(x)

  override def availableVariables(): Seq[T] = debruijnMapping.keySet.toSeq
}

object DeBruijnTranslation {

  private def updateMapWithFreshVars[T](m0: Map[T, Int], xs: Seq[T]) : Map[T, Int] = {
    if(xs.length == 0) {
      m0
    } else {
      val xsWithIds = xs zip Range(0, xs.length)
      val mShifted = m0.transform((x, id) => id+xs.length)

      xsWithIds.foldLeft[Map[T, Int]](mShifted) {
        (m, xWithId) => m.updated(xWithId._1, xWithId._2)
      }
    }
  }

  def freshTranslation[T](xs: Seq[T]) : DeBruijnTranslation[T] = {
    val res = updateMapWithFreshVars(Map.empty : Map[T, Int],  xs)

    new DeBruijnTranslation(res)
  }

}