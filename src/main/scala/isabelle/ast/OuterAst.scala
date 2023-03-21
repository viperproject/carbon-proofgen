package isabelle.ast

case class Theory(theoryName: String, importTheories: Seq[String], decls: Seq[OuterDecl])

sealed trait OuterDecl {
  def name: String
}

case class FunDecl(name: String, typ: Option[TypeIsa], equations: Seq[(Seq[Term], Term)]) extends OuterDecl
case class DefDecl(name: String, typ: Option[TypeIsa], equation: (Seq[Term], Term)) extends OuterDecl
case class AbbrevDecl(name: String, typ: Option[TypeIsa], equation: (Seq[Term], Term)) extends OuterDecl
case class LocaleDecl(name: String, context: ContextElem, body: Seq[OuterDecl]) extends OuterDecl
case class LemmaDecl(name: String, context: ContextElem, statements: Seq[Term], proof: Proof) extends OuterDecl

case object LemmaDecl {
  def apply(name: String, context: ContextElem, statement: Term, proof: Proof) : LemmaDecl =
    LemmaDecl(name, context, Seq(statement), proof)

  def apply(name: String, statement: Term, proof: Proof) : LemmaDecl =
    LemmaDecl(name, ContextElem.empty(), statement, proof)

}

case class LemmasDecl(name: String, thmNames: Seq[String]) extends OuterDecl

case class DeclareDecl(declaration: String) extends OuterDecl {
  override val name = "Declare"
}

case class ContextElem(fixedVariables: Seq[(TermIdent, TypeIsa)],
                       assumptions: Seq[(Option[String], Term)]) //label assumption

case object ContextElem {

  def empty() = ContextElem(Seq(), Seq())

  def onlyAssumptions(assumptions: Seq[(Option[String], Term)]) = ContextElem(Seq(), assumptions)

  def onlyAssumptionsNoLabels(assumptions: Seq[Term]) = ContextElem(Seq(), assumptions.map(assm => (None, assm)))

}
case class MLDecl(code: Seq[String], kind: MLKind) extends OuterDecl
{
  override val name = "ML"
}

sealed trait MLKind {
  val declId: String
}
case object MLNormal extends MLKind
{
  override val declId: String = "ML"
}
case object MLProof extends MLKind
{
  override val declId: String = "MLPrf"
}
case object MLVal extends MLKind
{
  override val declId: String = "MLVal"
}
