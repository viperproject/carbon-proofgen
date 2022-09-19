package isabelle.ast

case class Theory(theoryName: String, importTheories: Seq[String], decls: Seq[OuterDecl])

sealed trait OuterDecl {
  def name: String
}

case class FunDecl(name: String, typ: TypeIsa, equations: Seq[(Seq[Term], Term)]) extends OuterDecl
case class DefDecl(name: String, typ: TypeIsa, equation: (Seq[Term], Term)) extends OuterDecl
case class AbbrevDecl(name: String, typ: TypeIsa, equation: (Seq[Term], Term)) extends OuterDecl
case class LocaleDecl(name: String, context: ContextElem, body: Seq[OuterDecl]) extends OuterDecl
case class LemmaDecl(name: String, context: ContextElem, statements: Seq[Term]) extends OuterDecl
case class LemmasDecl(name: String, thmNames: Seq[String]) extends OuterDecl

case class ContextElem(fixedVariables: Seq[(TermIdent, TypeIsa)], assumptions: Seq[Term], assumptionLabels: Seq[String])