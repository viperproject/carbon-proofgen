package viper.carbon.proofgen

import isabelle.ast._
import viper.silver.{ast => sil}

object ViperIsaTerm {

  val nullLit = TermIdent("LNull")
  def intLit(i: BigInt) = TermApp(TermIdent("LInt"), IntConst(i))
  def boolLit(b: Boolean) = TermApp(TermIdent("LBool"), BoolConst(b))

  val nullExpr = litExpr(nullLit)

  def intExpr(i: BigInt) = litExpr(intLit(i))
  def boolExpr(b: Boolean) = litExpr(boolLit(b))

  val noPermExpr = litExpr(TermIdent("NoPerm"))
  val fullPermExpr = litExpr(TermIdent("WritePerm"))
  val wildCardExpr = TermIdent("Wildcard")

  def litExpr(lit: Term) = TermApp(TermIdent("ELit"), lit)

  def varExpr(i: Int) = TermApp(TermIdent("Var"), NatConst(i))

  val resultExpr = TermIdent("Result")

  def labelledOld(e: Term, lbl: String) = TermApp(TermApp(TermIdent("Old"), StringConst(lbl)), e)

  def condExp(cond: Term, thn: Term, els: Term) = TermApp(TermIdent("CondExp"), Seq(cond, thn, els))

  def unaryMinus(arg: Term) : Term = {
    TermApp(TermIdent("Unop"), TermIdent("Minus"), arg)
  }

  def unaryNot(arg: Term) : Term = {
    TermApp(TermIdent("Unop"), TermIdent("Not"), arg)
  }

  def binopPure(bop: sil.BinOp, left: Term, right: Term): Term = {
    val bopIsa =
      bop match {
        case sil.OrOp => TermIdent("Or")
        case sil.LeOp => TermIdent("Lte")
        case sil.LtOp => TermIdent("Lt")
        case sil.GeOp => TermIdent("Gte")
        case sil.GtOp => TermIdent("Gt")
        case sil.AddOp => TermIdent("Add")
        case sil.SubOp => TermIdent("Sub")
        case sil.DivOp => TermIdent("IntDiv")
        case sil.ModOp => TermIdent("Mod")
        case sil.MulOp => TermIdent("Mult")
        case sil.AndOp => TermIdent("And")
        case sil.ImpliesOp => TermIdent("BImp")
        case sil.PermGeOp => TermIdent("Gte")
        case sil.PermGtOp => TermIdent("Gt")
        case sil.PermLeOp => TermIdent("Lte")
        case sil.PermLtOp => TermIdent("Lt")
        case sil.PermAddOp => TermIdent("Add")
        case sil.PermMulOp => TermIdent("Mult")
        case sil.PermSubOp => TermIdent("Sub")
        case sil.IntPermMulOp => TermIdent("Mult")
        case sil.FracOp => TermIdent("PermDiv")
        case sil.PermDivOp => TermIdent("PermDiv")
      }

    binopPure(bopIsa, left, right)
  }

  def binopPure(bop: Term, left: Term, right: Term) : Term = {
    TermApp(TermIdent("Binop"), Seq(left, bop, right))
  }

  def binopImpure(bop: sil.BinOp, left: Term, right: Term): Term = {
    val bopIsa =
      bop match {
        case sil.AndOp => TermIdent("Star")
        case sil.ImpliesOp => TermIdent("Imp")
        case _ => sys.error(s"impure bop ${bop.toString} not supported")
      }

    TermApp(bopIsa, Seq(left, right))
  }

  def heapFieldAccess(rcv: Term, field: sil.Field) : Term = {
    TermApp(TermIdent("FieldAcc"), rcv, StringConst(field.name))
  }

  def fieldAccessPredicate(rcv: Term, field: sil.Field, perm: Term) : Term = {
    TermApp(TermIdent("Atomic"), TermApp(TermIdent("Acc"), Seq(rcv, StringConst(field.name), perm)))
  }

  def liftPureExpToAssertion(pureExp: Term) : Term = {
    TermApp(TermIdent("Atomic"), TermApp(TermIdent("Pure"), pureExp))
  }

  def liftExpressionToPermission(exp: Term, isWildcard: Boolean) : Term = {
    if(isWildcard) {
      ViperIsaTerm.wildCardExpr
    } else {
      TermApp(TermIdent("PureExp"), exp)
    }
  }

  def labelStmt(labelName: String) : Term = {
    TermApp(TermIdent("Label"), StringConst(labelName))
  }

  def localVarAssign(varIdx: Int, rhs: Term) : Term = {
    TermApp(TermIdent("LocalAssign"), IntConst(varIdx), rhs)
  }

  def fieldAssign(rcv: Term, field: sil.Field, rhs: Term) : Term = {
    TermApp(TermIdent("FieldAssign"), Seq(rcv, StringConst(field.name), rhs))
  }

  val skip : Term = TermIdent("Skip")

  def assume(e: Term) : Term =
    TermApp(TermIdent("Assume"), e)

  def assert(e: Term) : Term =
    TermApp(TermIdent("Assert"), e)

  def inhale(e: Term) : Term =
    TermApp(TermIdent("Inhale"), e)

  def exhale(e: Term) : Term =
    TermApp(TermIdent("Exhale"), e)

  def whileLoop(cond: Term, invs: Seq[Term], body: Term) : Term =
    TermApp(TermIdent("While"), Seq(cond, TermList(invs), body))

  def methodCall(methodName: String, args: Seq[Term], targets: Seq[Int]) : Term =
    TermApp(TermIdent("MethodCall"), Seq(TermList(targets map (i => NatConst(i))), StringConst(methodName), TermList(args)))

  def ifStmt(cond: Term, thn: Term, els: Term) : Term =
    TermApp(TermIdent("If"), Seq(cond, thn, els))

  def seq(s1: Term, s2: Term) : Term =
    TermApp(TermIdent("Seq"), s1, s2)

  def scope(ty: Term, body: Term) : Term =
    TermApp(TermIdent("Scope"), Seq(ty, body))

  def convertAstToProgramPoint(procBody: Term) =
    TermApp(TermIdent("convert_ast_to_program_point"), procBody)

  def methodsOfProgramProjection(prog: Term) = TermApp(TermIdent("program.methods"), prog)

}

object ViperTotalContext {
  def absvalInterpTotal(totalContext: Term) : Term = TermApp(TermIdent("absval_interp_total"), totalContext)
  def programTotal(totalContext: Term) : Term = TermApp(TermIdent("program_total"), totalContext)
  def rtypeInterp(totalContext: Term) : Term = TermApp(TermIdent("rtype_interp"), totalContext)

}
