package isabelle.ast

object MLUtil {


  def defineVal(varName: String, value: String) : String = s"val $varName = $value"

  def defineFun(funName: String, args: Seq[String], body: String) : String =
    s"fun $funName ${args.mkString(" ")} = $body"

  def app(fun: String, args: Seq[String]) : String = s"($fun ${args.mkString(" ")})"
  def app(fun: String, arg: String) : String = app(fun, Seq(arg))

  def createString(s: String) : String = s"\"$s\""

  def createTuple(elems: Seq[String]) : String = s"(${elems.mkString(", ")})"
  def createRecord(elems: Seq[(String, String)]) : String =
    s"{${elems.map({ case (name,value) => s"$name = $value" }).mkString(", ")}}"

  def createList(elems: Seq[String]) : String = s"[${elems.mkString(", ")}]"

  val none : String = "NONE"
  def some(x: String) = s"(SOME $x)"

  def isaToMLThm(isaThm: String) : String = s"@{thm $isaThm}"

  def isaToMLThms(isaThms: Seq[String]) : String = {
    if(isaThms.isEmpty) {
      "[]"
    } else {
      s"@{thms ${isaThms.mkString(" ")}}"
    }
  }

  def isaToMLThms(isaThms: String) : String = s"@{thms $isaThms}"

  val contextAniquotation = "@{context}"

  def simpAsmSolved(thms: String, ctxt : String = "") : String =
    s"(assm_full_simp_solved_with_thms_tac $thms" + (if(ctxt.isEmpty) { "" } else { " " + ctxt}) + ")"

  def simpAsm(thms: String, ctxt: String = "") : String =
    s"(simp_tac_with_thms $thms" + (if(ctxt.isEmpty) { "" } else { " " + ctxt}) + ")"

  def simpOnly(thms: String, ctxt: String = "") : String =
    s"(simp_only_tac $thms" + (if(ctxt.isEmpty) { "" } else { " " + ctxt})+ ")"

  def fastforceTac(thms: String, ctxt: String) : String =
    s"(fastforce_tac $ctxt $thms)"

  def resolveTac(ctxt: String, thms: String) : String=
    s"(resolve_tac $ctxt $thms)"

  def seqPrimeTac(tac1: String, tac2: String) : String = s"($tac1 THEN' $tac2)"

  def seqAllNewTac(tac1: String, tac2: String) : String = s"($tac1 THEN_ALL_NEW $tac2)"

  def tryPrimeTac(tac :String) : String = s"(TRY_TAC' $tac)"

  def lambda(args: Seq[String], body: String) : String = {
    val argsLambdaPrefix =
      args.foldLeft("")(
        { case (prefix, arg) => s"$prefix fn $arg =>" }
      )

    argsLambdaPrefix + body
  }

  def mlTacticToIsa(mlTactic: String) : String =
    s"tactic \\<open> $mlTactic \\<close>"

}
