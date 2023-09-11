package isabelle.ast

object IsaTermUtil {

  def mapOf(argList: Term) : Term = TermApp(TermIdent("map_of"), argList)

  def nthOption(argList: Term) : Term = TermApp(TermIdent("nth_option"), argList)

  def ran(partialFun: Term) : Term = TermApp(TermIdent("ran"), partialFun)

  def makeRecord(recordName: String, recordArgs: Seq[Term]) : Term = TermApp(TermIdent(recordName+".make"), recordArgs)

  def appendList(list1: Term, list2: Term) = TermApp(TermIdent("append"), list1, list2)

  def some(t: Term) = TermApp(TermIdent("Some"), t)

  val none = TermIdent("None")

  def fst(t: Term) = TermApp(TermIdent("fst"), t)
  def snd(t: Term) = TermApp(TermIdent("snd"), t)

  def convertOpt(termOpt: Option[Term]) : Term = termOpt.fold[Term](none)(t => some(t))

  val emptyMap : Term = TermIdent("Map.empty")

  def listAll(pred: Term, list: Term) : Term = TermApp(TermIdent("list_all"), pred, list)
  def listAll2(pred: Term, list1: Term, list2: Term) : Term = TermApp(TermIdent("list_all2"), Seq(pred, list1, list2))

  val undefined : Term = TermIdent("undefined")

  def injectiveOnDom(fun: Term, domain: Term) : Term = TermApp(TermIdent("inj_on"), fun, domain)

  def domainOfPartialFun(fun: Term) : Term = TermApp(TermIdent("dom"), fun)

}