package isabelle.ast

object MLUtil {


  def defineVal(varName: String, value: String) : String = s"val $varName = $value"

  def app(fun: String, args: Seq[String]) : String = s"($fun ${args.mkString(" ")})"
  def app(fun: String, arg: String) : String = app(fun, Seq(arg))

  def createTuple(elems: Seq[String]) : String = s"(${elems.mkString(", ")})"
  def createRecord(elems: Seq[(String, String)]) : String =
    s"{${elems.map({ case (name,value) => s"$name = $value" }).mkString(", ")}}"

  def createList(elems: Seq[String]) : String = s"[${elems.mkString(", ")}]"

  def isaToMLThm(isaThm: String) : String = s"@{thm $isaThm}"

  def isaToMLThms(isaThms: Seq[String]) : String = {
    if(isaThms.isEmpty) {
      "[]"
    } else {
      s"@{thms ${isaThms.mkString(" ")}}"
    }
  }

  val contextAniquotation = "@{context}"

  def simp(thms: String) : String =
    s"assm_full_simp_solved_with_thms_tac $thms"

  def mlTacticToIsa(mlTactic: String) : String =
    s"tactic \\<open> $mlTactic \\<close>"

}
