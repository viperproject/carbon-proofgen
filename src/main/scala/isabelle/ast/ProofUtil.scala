package isabelle.ast

case class Proof(methods: Seq[String])


object ProofUtil {

  def OF(thm: String, instThm: String) : String = s"$thm[OF $instThm]"

  def apply(tactic: String) : String = s"apply ($tactic)"

  def using(thm: String, tactic: String) : String = using(Seq(thm), tactic)

  def using(thm: Seq[String], tactic: String) : String  = s"using ${thm.mkString(" ")} $tactic"

  def by(tactic: String) : String = s"by $tactic"

  val simp = "simp"
  def simp(thm: String) : String = simp(Seq(thm))
  def simp(thms: Seq[String]) : String = s"(simp add: ${thms.mkString(" ")})"

  def rule(thm: String) : String = s"(rule $thm)"

}
