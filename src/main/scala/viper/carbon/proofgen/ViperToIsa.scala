package viper.carbon.proofgen

import isabelle.ast.TermIdent
import isabelle.{ast => isa}
import viper.carbon.boogie.LocalVarDecl
import viper.silver.ast.{NoInfo, NoPosition, TrueLit, WildcardPerm}
import viper.silver.{ast => sil}

object ViperToIsa {

  def translatePureExp(e: sil.Exp)(implicit varTranslation: VarTranslation[sil.LocalVar]) : isa.Term = {
    e match {
      case sil.IntLit(i) => ViperIsaTerm.intExpr(i)
      case sil.BoolLit(b) => ViperIsaTerm.boolExpr(b)
      case sil.NullLit() => ViperIsaTerm.nullExpr
      case l@sil.LocalVar(_, _) =>
        varTranslation.translateVariable(l) match {
          case Some(t) => t
          case None => sys.error(s"Could not translate variable $l")
        }
      case sil.Result(_) =>
        ViperIsaTerm.resultExpr
      case sil.FieldAccess(rcv, f) =>
        ViperIsaTerm.heapFieldAccess(translatePureExp(rcv), f)
      case sil.InhaleExhaleExp(_, _) =>
        sys.error("inhale exhale expression not supported")
      case sil.PredicateAccess(_, _) =>
        sys.error("predicates not supported")
      case sil.Unfolding(_, exp) =>
        sys.error("unfolding not supported")
      case sil.Applying(_, exp) =>
        sys.error("applying not supported")
      case sil.Old(exp) =>
        sys.error("old expression not supported")
      case sil.LabelledOld(exp, oldLabel) =>
        ViperIsaTerm.labelledOld(translatePureExp(exp), oldLabel)
      case sil.Let(lvardecl, exp, body) =>
        sys.error("let expressions not supported")
      case sil.CondExp(cond, thn, els) =>
        val condTerm = translatePureExp(cond)
        val thnTerm = translatePureExp(thn)
        val elsTerm = translatePureExp(els)

        ViperIsaTerm.condExp(condTerm, thnTerm, elsTerm)
      case sil.Exists(vars, triggers, exp) =>
        sys.error("quantifiers not supported")
      case sil.Forall(vars, triggers, exp) =>
        sys.error("quantifiers not supported")
      case sil.ForPerm(variables, accessRes, body) =>
        sys.error("forperm expressions not supported")
      case sil.WildcardPerm() => ViperIsaTerm.wildCardExpr
      case sil.FullPerm() => ViperIsaTerm.fullPermExpr
      case sil.NoPerm() => ViperIsaTerm.noPermExpr
      case sil.EpsilonPerm() =>
        sys.error("epsilon permission not supported")
      case sil.PermMinus(e) =>
        ViperIsaTerm.unaryMinus(translatePureExp(e))
      case sil.CurrentPerm(e) =>
        sys.error("permission introspection not supported")
      case sil.FractionalPerm(p, q) =>
        ViperIsaTerm.binopPure(sil.PermDivOp, translatePureExp(p), translatePureExp(q))
      case sil.AccessPredicate(_, _) =>
        sys.error("not handled by expression module")
      case sil.EqCmp(left, right) =>
        val leftTerm = translatePureExp(left)
        val rightTerm = translatePureExp(right)

        ViperIsaTerm.binopPure(TermIdent("Eq"), leftTerm, rightTerm)
      case sil.NeCmp(left, right) =>
        val leftTerm = translatePureExp(left)
        val rightTerm = translatePureExp(right)

        ViperIsaTerm.binopPure(TermIdent("Neq"), leftTerm, rightTerm)
      case sil.DomainBinExp(left, op, right) => {
        val leftTerm = translatePureExp(left)
        val rightTerm = translatePureExp(right)

        ViperIsaTerm.binopPure(op, leftTerm, rightTerm)
      }
      case sil.Minus(exp) =>
        ViperIsaTerm.unaryMinus(translatePureExp(exp))
      case sil.Not(exp) =>
        ViperIsaTerm.unaryNot(translatePureExp(exp))
      case sil.FuncApp(_, _) =>
        sys.error("functions not supported")
      case sil.DomainFuncApp(_, _, _) =>
        sys.error("domain function application not supported")
      case sil.BackendFuncApp(_, _) =>
        sys.error("backend function application not supported")
      case sil.EmptySeq(_) =>
        sys.error("sequences not supported")
      case sil.ExplicitSeq(_) =>
        sys.error("sequences not supported")
      case sil.RangeSeq(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqAppend(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqIndex(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqTake(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqDrop(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqContains(_, _) =>
        sys.error("sequences not supported")
      case sil.SeqUpdate(_, _, _) =>
        sys.error("sequences not supported")
      case sil.SeqLength(_) =>
        sys.error("sequences not supported")

      case sil.EmptySet(_) =>
        sys.error("sets not supported")
      case sil.ExplicitSet(_) =>
        sys.error("sets not supported")
      case sil.EmptyMultiset(_) =>
        sys.error("sets not supported")
      case sil.ExplicitMultiset(_) =>
        sys.error("sets not supported")
      case sil.AnySetUnion(_, _) =>
        sys.error("sets not supported")
      case sil.AnySetIntersection(_, _) =>
        sys.error("sets not supported")
      case sil.AnySetSubset(_, _) =>
        sys.error("sets not supported")
      case sil.AnySetMinus(_, _) =>
        sys.error("sets not supported")
      case sil.AnySetContains(_, _) =>
        sys.error("sets not supported")
      case sil.AnySetCardinality(_) =>
        sys.error("sets not supported")

      case _: sil.EmptyMap =>
        sys.error("maps not supported")
      case _: sil.ExplicitMap =>
        sys.error("maps not supported")
      case _: sil.Maplet =>
        sys.error("maps not supported")
      case _: sil.MapCardinality =>
        sys.error("maps not supported")
      case _: sil.MapContains =>
        sys.error("maps not supported")
      case _: sil.MapDomain =>
        sys.error("maps not supported")
      case _: sil.MapRange =>
        sys.error("maps not supported")
      case _: sil.MapLookup =>
        sys.error("maps not supported")
      case _: sil.MapUpdate =>
        sys.error("maps not supported")

      case _ => sys.error("Viper expression didn't match any existing case.")
    }
  }

  def translatePermission(e: sil.Exp)(implicit varTranslation: VarTranslation[sil.LocalVar]) : isa.Term = {
    val t = translatePureExp(e)
    ViperIsaTerm.liftExpressionToPermission(t, e.isInstanceOf[sil.WildcardPerm])
  }

  def translateAssertion(e: sil.Exp)(implicit varTranslation: VarTranslation[sil.LocalVar]) : isa.Term = {
    e match {
      case sil.FieldAccessPredicate(loc, perm) =>
        ViperIsaTerm.fieldAccessPredicate(translatePureExp(loc.rcv), loc.field, translatePermission(perm))
      case sil.PredicateAccessPredicate(_, _) => sys.error("predicates not supported")
      /** There is a choice to be made here: For the [[sil.And]] and [[sil.Implies]] nodes, if both arguments are pure,
        * then one could also directly represent the nodes using their pure forms. However, Carbon treats these pure forms
        * the same way as it does the impure forms. This is the reason why we use the impure forms here irrespective
        * of whether the arguments are pure. */
      case a@sil.And(left, right) => ViperIsaTerm.binopImpure(a.funct, translateAssertion(left), translateAssertion(right))
      case e@sil.Implies(cond, exp) => ViperIsaTerm.binopImpure(e.funct, translateAssertion(cond), translateAssertion(exp))
      case sil.MagicWand(_, _) => sys.error("magic wands not supported")
      case _  if e.isPure => ViperIsaTerm.liftPureExpToAssertion(translatePureExp(e))
      case _ => sys.error(e.toString + " not supported")
    }
  }

  def translateStmt(stmt: sil.Stmt)(implicit varTranslation: VarTranslation[sil.LocalVar]) : isa.Term = {
    stmt match {
      case sil.LocalVarAssign(lhs, rhs) =>
        varTranslation.translateVariableId(lhs) match {
          case Some(id) => ViperIsaTerm.localVarAssign(id, translatePureExp(rhs))
          case None => sys.error(s"could not translate variable $lhs")
        }
      case sil.FieldAssign(lhs, rhs) =>
        val rcvTerm = translatePureExp(lhs.rcv)
        val rhsTerm = translatePureExp(rhs)

        ViperIsaTerm.fieldAssign(rcvTerm, lhs.field, rhsTerm)
      case sil.Fold(e) => sys.error("do not support fold")
      case sil.Unfold(e) => sys.error("do not support unfold")
      case sil.Inhale(e) => ViperIsaTerm.inhale(translateAssertion(e))
      case sil.Exhale(e) => ViperIsaTerm.exhale(translateAssertion(e))
      case sil.Assert(e) => ViperIsaTerm.assert(translateAssertion(e))
      case sil.Seqn(stmts, scopedDecls) =>

        if(stmts.length == 0) {
          ViperIsaTerm.skip
        } else {
          val localVars = scopedDecls.collect { case variable: sil.LocalVarDecl => variable.localVar }
          val updatedTranslation = if(localVars.length > 0) { varTranslation.addFreshVariables(localVars) } else { varTranslation }
          val stmtsTerm = stmts map (s => translateStmt(s)(updatedTranslation))

          val stmtWithoutScopes =
            if(stmtsTerm.length == 1) {
              stmtsTerm(0)
            } else {
            /* using foldRight in order to get more natural nesting [s1,s2,s3,...] -> Seq(s1, Seq(s2, Seq(s3,...))) instead of
               Seq(Seq(Seq(s1, s2), s3),...)
             */
              stmtsTerm.dropRight(1).foldRight(stmtsTerm.last) {
                (t, res) => ViperIsaTerm.seq(t, res)
              }
            }

          val stmtWithScopes =
            localVars.foldRight(stmtWithoutScopes){
              (localVar, stmt) => ViperIsaTerm.scope(ViperIsaType.translate(localVar.typ), stmt)
            }

          stmtWithScopes
        }
      case sil.MethodCall(methodName, args, targets) => sys.error("do not support method calls")
      case sil.While(cond, invs, body) =>
        ViperIsaTerm.whileLoop(
          translatePureExp(cond),
          invs map translateAssertion,
          translateStmt(body))
      case sil.If(cond, thn, els) =>
        ViperIsaTerm.ifStmt(
          translatePureExp(cond),
          translateStmt(thn),
          translateStmt(els)
        )
      case sil.Label(name, invs) =>
        if(invs != null && !invs.isEmpty) {
          sys.error("do not support invariants at labels")
        }

        ViperIsaTerm.labelStmt(name)
      case sil.Goto(_) => sys.error("do not support gotos")
      case sil.Package(wand, proof) => sys.error("do not support wands")
      case sil.Apply(wand) => sys.error("do not support wands")
    }
  }

}
