package isabelle.ast

case class Proof(methods: Seq[String])


object ProofUtil {

  def thmWithAttributeSingleValue(thm: String, attributeName: String, attributeValue: String) = s"$thm[$attributeName $attributeValue]"
  def thmWithAttributeMultipleValues(thm: String, attributeName: String, attributeValues: Seq[String]) = s"$thm[$attributeName ${attributeValues.mkString(" ")}]"

  def OF(thm: String, instThm: String) : String = thmWithAttributeSingleValue(thm, "OF", instThm)
  def OF(thm: String, instThms: Seq[String]) : String = thmWithAttributeMultipleValues(thm, "OF", instThms)
  def multiAttributes(thm: String, attributesWithValues: Seq[(String, Seq[String])]) : String = {
    val attributeWithValuesOutput =
      attributesWithValues.map(
        {
          case (attributeName, attributeValues) => s"$attributeName ${attributeValues.mkString(" ")}"
        }
      ).mkString(",")

    s"$thm[$attributeWithValuesOutput]"
  }

  def simplified(thm: String, simpThm: String) : String = thmWithAttributeSingleValue(thm, "simplified", simpThm)
  def simplified(thm: String, simpThms: Seq[String]) : String = thmWithAttributeMultipleValues(thm, "simplified", simpThms)

  def where(thm: String, schematicVar: String, instantiation: String) : String = s"$thm[where ?$schematicVar=\"$instantiation\"]"
  def where(thm: String, schematicVar: String, instantiation: Term) : String =
    where(thm, schematicVar, IsaPrettyPrinter.prettyPrint(instantiation))

  def applyTac(tactic: String) : String = s"apply ($tactic)"

  def preferTac(subgoalId: Int) = s"prefer $subgoalId"

  def mapApplyTac(tactics: Seq[String]) : Seq[String] = tactics.map(tac => applyTac(tac))

  def using(thm: String, tactic: String) : String = using(Seq(thm), tactic)

  def using(thm: Seq[String], tactic: String) : String  = s"using ${thm.mkString(" ")} $tactic"

  def byTac(tactic: String) : String = s"by $tactic"

  val doneTac : String = "done"

  val simp = "simp"

  val simpTac : String = simpTacGeneral(Seq())
  def simpTac(thm: String) : String = simpTacGeneral(Seq((SimpAddModifier, Seq(thm))))
  def simpTac(thms: Seq[String]) : String = simpTacGeneral(Seq((SimpAddModifier, thms)))
  def simpTacOnly(thm: String) : String = simpTacGeneral(Seq((SimpOnlyModifier, Seq(thm))))
  def simpTacOnly(thms: Seq[String]) : String = simpTacGeneral(Seq((SimpOnlyModifier, thms)))
  def simpTacDel(thm: String) : String = simpTacGeneral(Seq((SimpDelModifier, Seq(thm))))
  def simpTacDel(thms: Seq[String]) : String = simpTacGeneral(Seq((SimpDelModifier, thms)))
  def simpTacAddDel(addThms: Seq[String], delThms: Seq[String]) = simpTacGeneral(Seq((SimpAddModifier, addThms), (SimpDelModifier, delThms)))

  def simpTacGeneral(modifierValues: Seq[(SimpModifier, Seq[String])]) : String = {
    if(modifierValues.isEmpty) {
      "simp"
    } else {
      s"(simp ${modifierValues.map( modVal => modVal._1.name + ": " + modVal._2.mkString(" ")).mkString(" ")})"
    }
  }

  def ruleTac(thm: String) : String = s"(rule $thm)"

  def eruleTac(thm: String) : String = s"(erule $thm)"

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

  val blastTac: String = "blast"

  def repeatTac(tac: String) = s"($tac)+"

  val assumeTac : String = "assumption"

  val reflTac : String = ruleTac("HOL.refl")

  def cutTac(thms: Seq[String]) = s"(cut_tac ${thms.mkString(" ")})"
  def cutTac(thm: String) = s"(cut_tac $thm)"

}

sealed trait SimpModifier
{
  def name: String
}

case object SimpAddModifier extends SimpModifier {
  override val name: String = "add"
}

case object SimpOnlyModifier extends SimpModifier {
  override val name: String = "only"
}

case object SimpDelModifier extends SimpModifier {
  override val name: String = "del"
}