package isabelle.ast

case class Proof(methods: Seq[String])


object ProofUtil {

  def thmWithAttributeSingleValue(thm: String, attributeName: String, attributeValue: String) = s"$thm[$attributeName $attributeValue]"
  def thmWithAttributeMultipleValues(thm: String, attributeName: String, attributeValues: Seq[String]) = s"$thm[$attributeName ${attributeValues.mkString(" ")}]"

  def OF(thm: String, instThm: String) : String = thmWithAttributeSingleValue(thm, "OF", instThm)
  def OF(thm: String, instThms: Seq[String]) : String = thmWithAttributeMultipleValues(thm, "OF", instThms)

  def simplified(thm: String, simpThm: String) : String = thmWithAttributeSingleValue(thm, "simplified", simpThm)
  def simplified(thm: String, simpThms: Seq[String]) : String = thmWithAttributeMultipleValues(thm, "simplified", simpThms)

  def where(thm: String, schematicVar: String, instantiation: String) : String = s"$thm[where ?$schematicVar=\"$instantiation\"]"
  def where(thm: String, schematicVar: String, instantiation: Term) : String =
    where(thm, schematicVar, IsaPrettyPrinter.prettyPrint(instantiation))

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

  def introTac(thm: String) : String = s"(intro $thm)"

  def unfoldTac(thm: String) : String = unfoldTac(Seq(thm))
  def unfoldTac(thms: Seq[String]) : String = s"(unfold ${thms.mkString(" ")})"

  val fastforceTac : String = fastforceTacWithSimps(Seq())

  private def modifierArgument(modifier: String, args: Seq[String]): String = {
    if(args.isEmpty) { "" } else {s"$modifier: ${args.mkString(" ")}"}
  }

  def builtInTac(tacName: String, intros: Seq[String], elims: Seq[String], simps: Seq[String]) : String = {
    "(" + tacName + s"${modifierArgument(" intro", intros)}"+ s"${modifierArgument(" elim", elims)}" + s"${modifierArgument(" simp", simps)})"
  }
  def fastforceTacWithIntros(thms: Seq[String]) : String = builtInTac("fastforce", thms, Seq(), Seq())
  def fastforceTacWithSimps(thms: Seq[String]) : String = builtInTac("fastforce", Seq(), Seq(), thms)

  def forceTacWithIntros(thms: Seq[String]) : String = builtInTac("force", thms, Seq(), Seq())
  def forceTacWithSimps(thms: Seq[String]) : String = builtInTac("force", Seq(), Seq(), thms)

  def repeatTac(tac: String) = s"($tac)+"

  val assumeTac : String = "assumption"

  val reflTac : String = ruleTac("HOL.refl")

}
