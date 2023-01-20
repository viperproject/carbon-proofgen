package isabelle.ast

//region Identifier
sealed trait Identifier

case class SimpleIdentifier(s: String) extends Identifier
{
  override def toString: String = s
}

case object Wildcard extends Identifier
{
  override def toString: String = "_"
}
//endregion

//region Term
sealed trait Term {
  override def toString: String = IsaPrettyPrinter.prettyPrint(this)
}

case class TermIdent(id: Identifier) extends Term

case object TermIdent {

  def apply(s: String) : TermIdent  = {
    TermIdent(SimpleIdentifier(s))
  }

}

case class TermApp(fun: Term, arg: Seq[Term]) extends Term

object TermApp {
  //variadic Term* does not seem to work as expected
  def apply(fun: Term, arg: Term) : TermApp = {
    TermApp(fun, Seq(arg))
  }

  def apply(fun: Term, arg1: Term, arg2: Term) : TermApp = {
    TermApp(fun, Seq(arg1, arg2))
  }
}

case class TermWithExplicitType(t: Term, ty: TypeIsa) extends Term

case class TermList(list: Seq[Term]) extends Term

case class TermTuple(list: Seq[Term]) extends Term

object TermTuple {
  def apply(t1: Term, t2: Term) : TermTuple = TermTuple(Seq(t1, t2))

}

sealed trait QuantifierKind
case object All extends QuantifierKind
case object Exists extends QuantifierKind
case object MetaAll extends QuantifierKind
case object Lambda extends QuantifierKind

case class TermQuantifier(quantifierKind: QuantifierKind, boundVars: Seq[Identifier], body: Term) extends Term

sealed trait BinaryOpCode
case object MetaImp extends BinaryOpCode
case object Eq extends BinaryOpCode
case object Neq extends BinaryOpCode
case object Lt extends BinaryOpCode
case object Le extends BinaryOpCode
case object Gt extends BinaryOpCode
case object Ge extends BinaryOpCode
case object And extends BinaryOpCode
case object Or extends BinaryOpCode
case object Implies extends BinaryOpCode
case object Add extends BinaryOpCode
case object Sub extends BinaryOpCode
case object Mul extends BinaryOpCode

sealed trait UnaryOpCode
case object Not extends UnaryOpCode

case class TermBinary(bop: BinaryOpCode, left: Term, right: Term) extends Term

case object TermBinary {
  def eq(t1: Term, t2: Term) = TermBinary(Eq, t1, t2)
  def implies(t1: Term, t2: Term) = TermBinary(Implies, t1, t2)

}


case class TermUnary(uop: UnaryOpCode, arg: Term) extends Term

sealed trait Const extends Term

case class IntConst(i: BigInt) extends Const
case class NatConst(n: BigInt) extends Const
case class BoolConst(b: Boolean) extends Const
case class StringConst(s: String) extends Const
//endregion

//region TypeIsa
sealed trait TypeIsa

case class VarType(x: String) extends TypeIsa

case class ArrowType(argType: TypeIsa, resType: TypeIsa) extends TypeIsa

case class DataType(name: String, args: Seq[TypeIsa]) extends TypeIsa

object DataType {
  def apply(name: String, arg: TypeIsa) : DataType = DataType(name, Seq(arg))
}

case class TupleType(args: Seq[TypeIsa]) extends TypeIsa

//endregion

//region PrimitiveType
sealed trait PrimitiveType extends TypeIsa

case object BoolType extends PrimitiveType
case object NatType extends PrimitiveType
case object IntType extends PrimitiveType
//endregion