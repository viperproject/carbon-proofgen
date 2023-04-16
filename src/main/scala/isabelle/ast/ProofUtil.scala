package isabelle.ast

case class Proof(methods: Seq[String])


object ProofUtil {

  def OF(thm: String, instThm: String) : String = s"$thm[OF $instThm]"
  def OF(thm: String, instThms: Seq[String]) : String = s"$thm[OF ${instThms.mkString(" ")}]"

  def where(thm: String, schematicVar: String, instantiation: String) : String = s"$thm[where ?$schematicVar=\"$instantiation\"]"

  def applyTac(tactic: String) : String = s"apply ($tactic)"

  def using(thm: String, tactic: String) : String = using(Seq(thm), tactic)

  def using(thm: Seq[String], tactic: String) : String  = s"using ${thm.mkString(" ")} $tactic"

  def byTac(tactic: String) : String = s"by $tactic"

  val doneTac : String = "done"

  val simp = "simp"

  def simpAddModifier(onlyFlag: Boolean): String = {
    if(onlyFlag) { "only" } else { "add"}
  }

  val simpTac : String = simpTacGeneral(Seq(), false)
  def simpTac(thm: String) : String = simpTacGeneral(Seq(thm), false)
  def simpTac(thms: Seq[String]) : String = simpTacGeneral(thms, false)
  def simpTacOnly(thm: String) : String = simpTacGeneral(Seq(thm), true)

  def simpTacOnly(thms: Seq[String]) : String = simpTacGeneral(thms, true)

  def simpTacGeneral(thms: Seq[String], onlyFlag: Boolean) : String = {
    if(thms.isEmpty) {
      "simp"
    } else {
      s"(simp ${simpAddModifier(onlyFlag)}: ${thms.mkString(" ")})"
    }
  }

  def ruleTac(thm: String) : String = s"(rule $thm)"

  def unfoldTac(thm: String) : String = unfoldTac(Seq(thm))
  def unfoldTac(thms: Seq[String]) : String = s"(unfold ${thms.mkString(" ")})"

  val fastforceTac : String = fastforceTacWithSimps(Seq())

  private def modifierArgument(modifier: String, args: Seq[String]): String = {
    if(args.isEmpty) { "" } else {s"$modifier: ${args.mkString(" ")}"}
  }

  def fastforceTac(intros: Seq[String], elims: Seq[String], simps: Seq[String]) : String = {
    "(fastforce"+ s" ${modifierArgument("intro", intros)}"+ s" ${modifierArgument("elim", elims)}" + s" ${modifierArgument("simp", simps)})"
  }
  def fastforceTacWithIntros(thms: Seq[String]) : String = fastforceTac(thms, Seq(), Seq())
  def fastforceTacWithSimps(thms: Seq[String]) : String = fastforceTac(Seq(), Seq(), thms)

  val assumeTac : String = "assumption"

}
