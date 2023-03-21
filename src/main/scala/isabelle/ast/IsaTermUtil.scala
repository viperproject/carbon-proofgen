package isabelle.ast

object IsaTermUtil {

  def mapOf(argList: Term) : Term = TermApp(TermIdent("map_of"), argList)

  def makeRecord(recordName: String, recordArgs: Seq[Term]) : Term = TermApp(TermIdent(recordName+".make"), recordArgs)

  def appendList(list1: Term, list2: Term) = TermApp(TermIdent("append"), list1, list2)

  def some(t: Term) = TermApp(TermIdent("Some"), t)

  val none = TermIdent("None")

  def fst(t: Term) = TermApp(TermIdent("fst"), t)
  def snd(t: Term) = TermApp(TermIdent("snd"), t)

  def convertOpt(termOpt: Option[Term]) : Term = termOpt.fold[Term](none)(t => some(t))

  val emptyMap : Term = TermIdent("Map.empty")

  def listAll(pred: Term, list: Term) : Term = TermApp(TermIdent("list_all"), pred, list)

}