// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules._
import viper.silver.{ast => sil}
import viper.carbon.boogie._
import viper.carbon.verifier.Verifier
import viper.carbon.boogie.Implicits._
import viper.carbon.proofgen.hints.{AtomicInhaleHint, CondInhaleHint, FieldAccessPredicateInhaleHint, GoodStateAfterInhaleHint, ImpInhaleHint, InhaleBodyProofHint, InhaleComponentProofHint, InhaleProofHint, NotSupportedAtomicInhaleHint, PureExpInhaleHint, StarInhaleHint, TrivialInhaleHint}
import viper.silver.verifier.PartialVerificationError

/**
 * The default implementation of a [[viper.carbon.modules.InhaleModule]].

 */
class DefaultInhaleModule(val verifier: Verifier) extends InhaleModule with StatelessComponent {

  import verifier._
  import expModule._
  import stateModule._
  import mainModule._

  def name = "Inhale module"

  override def start(): Unit = {
    register(this)
  }

  override def inhale(exps: Seq[(sil.Exp, PartialVerificationError)], addDefinednessChecks: Boolean, statesStackForPackageStmt: List[Any] = null, insidePackageStmt: Boolean = false): (Stmt, InhaleProofHint) = {
    val current_state = stateModule.state
    if(insidePackageStmt && !addDefinednessChecks) { // replace currentState with the correct state in which the inhale occurs during packaging the wand
      stateModule.replaceState(statesStackForPackageStmt(0).asInstanceOf[StateRep].state)
    }


    val (mainStmt, hints) =
        (exps map (e => inhaleConnective(e._1.whenInhaling, e._2, addDefinednessChecks = addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt = insidePackageStmt))).unzip


    val stmt = mainStmt ++ assumeGoodState

    val resultHint =
      if(hints.isEmpty) {
        InhaleProofHint(GoodStateAfterInhaleHint(TrivialInhaleHint), addDefinednessChecks)
      } else {
        InhaleProofHint(GoodStateAfterInhaleHint(InhaleProofHint.combineBodyHintsIntoBodyHint(hints)), addDefinednessChecks)
      }

    if(insidePackageStmt && !addDefinednessChecks) {
         /* all the assumptions made during packaging a wand (except assumptions about the global state before the package statement)
          * should be replaced by updates to state booleans (see documentation for 'exchangeAssumesWithBoolean') */
      stateModule.replaceState(current_state)
      (wandModule.exchangeAssumesWithBoolean(stmt, statesStackForPackageStmt.head.asInstanceOf[StateRep].boolVar), resultHint)
    } else {
      (stmt, resultHint)
    }
  }

  def containsFunc(exp: sil.Exp): Boolean = {
    var res = false
    exp visit {
      case _: sil.FuncApp => res = true
    }
    res
  }

  /**
   * Inhales Viper expression connectives (such as logical and/or) and forwards the
   * translation of other expressions to the inhale components.
   */
  private def inhaleConnective(e: sil.Exp, error: PartialVerificationError, addDefinednessChecks: Boolean, statesStackForPackageStmt: List[Any] = null, insidePackageStmt: Boolean = false): (Stmt, InhaleBodyProofHint) = {

    def maybeDefCheck(eDef: sil.Exp) : Stmt = { if(addDefinednessChecks) checkDefinedness(eDef, error, insidePackageStmt = insidePackageStmt) else Statements.EmptyStmt }

    def maybeFreeAssumptions(eAssm: sil.Exp) : Stmt = {
      /* definedness checks include free assumptions, so only add free assumption if no definedness checks were made
         GP: Currently, inhale during packaging a wand still requires these additional free assumptions even if definedness
             checks are made. For instance, the example wands/regression/issue029.vpr in the Viper test suite requires
             this. That's why there is an additional conjunct in the if condition. However, this special case during
             packaging a wand needs to be revisited.
       */
      if(addDefinednessChecks && !insidePackageStmt) {
        Nil
      } else {
        MaybeCommentBlock("Free assumptions (inhale module)", allFreeAssumptions(eAssm))
      }
    }

    val (resStmt, hint) : (Stmt, InhaleBodyProofHint) =
      e match {
        case sil.And(e1, e2) =>
          val (stmt1, hint1) = inhaleConnective(e1, error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)
          val (stmt2, hint2) = inhaleConnective(e2, error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)
          (stmt1 :: stmt2 :: Nil, StarInhaleHint(hint1, hint2))
        case sil.Implies(e1, e2) =>
          val defCheck = maybeDefCheck(e1)
          val lhsTranslation = if(insidePackageStmt && addDefinednessChecks) { wandModule.getCurOpsBoolvar() ==> translateExpInWand(e1) } else { translateExp(e1) }
          val (rhsTranslation, rhsHint) = inhaleConnective(e2, error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)

          (defCheck ++ If(lhsTranslation, rhsTranslation, Statements.EmptyStmt), ImpInhaleHint(e1, rhsHint))
        case sil.CondExp(c, e1, e2) =>
          val defCheck = maybeDefCheck(c)
          val condTranslation = if(insidePackageStmt && addDefinednessChecks) { wandModule.getCurOpsBoolvar() ==> translateExpInWand(c) } else { translateExp(c) }

          val (thnTranslation, thnHint) = inhaleConnective(e1, error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)
          val (elsTranslation, elsHint) = inhaleConnective(e2, error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)

          (defCheck ++ If(condTranslation, thnTranslation, elsTranslation), CondInhaleHint(c, thnHint, elsHint))
        case sil.Let(declared,boundTo,body) if !body.isPure || addDefinednessChecks =>
        {
          val defCheck = maybeDefCheck(boundTo)
          val u = env.makeUniquelyNamed(declared) // choose a fresh binder
          env.define(u.localVar)
          val resStmt =
            defCheck ::
            Assign(translateLocalVar(u.localVar),translateExp(boundTo)) ::
              inhaleConnective(body.replace(declared.localVar, u.localVar), error, addDefinednessChecks, statesStackForPackageStmt, insidePackageStmt)._1 ::
              {
                env.undefine(u.localVar)
                Nil
              }

          (resStmt, NotSupportedAtomicInhaleHint)
        }
        case _ =>
          def transformStmtInsidePackage(s: Stmt): Stmt = {
            if(insidePackageStmt && addDefinednessChecks) {
              wandModule.exchangeAssumesWithBoolean(s, statesStackForPackageStmt.head.asInstanceOf[StateRep].boolVar)
            } else {
              s
            }
          }
          val definednessChecks = maybeDefCheck(e)
          val freeAssms = maybeFreeAssumptions(e)
          val (stmt, componentHints) = (components map (_.inhaleExp(e, error))).unzip
          if (stmt.children.isEmpty)
            sys.error(s"missing translation for inhaling of $e")

          //do not transform definednessChecks inside package (backwards compatible with older version)
          val retStmt =
            transformStmtInsidePackage(if (containsFunc(e)) Seq(assumeGoodState) else Seq()) ++
            definednessChecks ++
            transformStmtInsidePackage(stmt ++ (if (e.isPure) Seq() else Seq(assumeGoodState))) ++
            freeAssms
          //(if (containsFunc(e)) assumeGoodState else Seq[Stmt]()) ++ stmt ++ (if (e.isPure) Seq[Stmt]() else assumeGoodState)

          val atomicHint = constructAtomicHint(e, componentHints.flatten)
          val resultHint = if(e.isPure) { atomicHint } else { GoodStateAfterInhaleHint(atomicHint) }

          // if we are inside package statement, then all assumptions should be replaced with conjinctions with ops.boolVar
          (retStmt, resultHint)
      }
    if(insidePackageStmt && addDefinednessChecks) {
      (If(wandModule.getCurOpsBoolvar(), resStmt, Statements.EmptyStmt), hint)
    } else {
      (resStmt, hint)
    }
  }

  private def constructAtomicHint(atomicExp: sil.Exp, componentHints: Seq[InhaleComponentProofHint]) : AtomicInhaleHint = {
    atomicExp match {
      case acc: sil.FieldAccessPredicate => FieldAccessPredicateInhaleHint(acc, componentHints)
      case _ if atomicExp.isPure => PureExpInhaleHint(atomicExp, componentHints)
      case _ => sys.error(s"Cannot construct atomic inhale hint for ${atomicExp.toString()}")
    }
  }

  override def inhaleExp(e: sil.Exp, error: PartialVerificationError): (Stmt, Seq[InhaleComponentProofHint]) = {
    val resStmt : Stmt =
      if (e.isPure) {
        Assume(translateExp(e))
      } else {
        Nil
      }
    (resStmt, Seq())
  }
}
