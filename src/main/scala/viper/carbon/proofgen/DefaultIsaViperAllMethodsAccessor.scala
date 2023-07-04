package viper.carbon.proofgen
import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultIsaViperAllMethodsAccessor(theoryName: String, override val methodLookupFun: TermIdent, methodAccessorMap: Map[String, IsaViperMethodAccessor]) extends IsaViperAllMethodsAccessor {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override def lookupLemmaName(methodName: String): String = qualifyName(DefaultIsaViperAllMethodsAccessor.lookupLemmaName(methodName))

  override def methodAccessor(methodName: String): IsaViperMethodAccessor = methodAccessorMap.get(methodName).get

}

object DefaultIsaViperAllMethodsAccessor {

  def lookupLemmaName(methodName: String): String = s"${methodName}_lookup_lemma"

}
