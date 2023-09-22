package viper.carbon.proofgen
import isabelle.ast.{IsaUtil, TermIdent}
import viper.silver.{ast => sil}

case class DefaultIsaViperAllMethodsAccessor(theoryName: String,
                                             override val methodOrder: Seq[sil.Method],
                                             override val methodLookupFun: TermIdent,
                                             methodAccessorMap: Map[String, IsaViperMethodAccessor]) extends IsaViperAllMethodsAccessor {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override def lookupLemmaName(methodName: String): String = qualifyName(DefaultIsaViperAllMethodsAccessor.lookupLemmaName(methodName))

  override def methodAccessor(methodName: String): IsaViperMethodAccessor = methodAccessorMap.get(methodName).get

}

object DefaultIsaViperAllMethodsAccessor {

  def lookupLemmaName(methodName: String): String = s"${methodName}_lookup_lemma"

}
