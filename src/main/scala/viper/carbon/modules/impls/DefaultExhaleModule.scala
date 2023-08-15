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
import Implicits._
import viper.carbon.modules.components.DefinednessState
import viper.carbon.proofgen.hints.{CondExhaleHint, DefaultExhaleProofHint, ExhaleBodyProofHint, ExhaleComponentProofHint, ExhaleProofHint, FieldAccessPredicateExhaleHint, ForallExhaleHint, ImpExhaleHint, NotSupportedExhaleHint, PureExpExhaleHint, StarExhaleHint, UnfoldingExhaleHint}
import viper.silver.ast.utility.Expressions
import viper.silver.verifier.PartialVerificationError
import viper.silver.verifier.reasons._

/**
 * The default implementation of a [[viper.carbon.modules.ExhaleModule]].
 */
class DefaultExhaleModule(val verifier: Verifier) extends ExhaleModule {

  import verifier._
  import expModule._
  import permModule._
  import heapModule._
  import mainModule._
  import stateModule._

  def name = "Exhale module"

  override def reset = { }

  override def start(): Unit = {
    register(this)
  }

  var nestedExhaleId = 0

  override def exhale(exps: Seq[(sil.Exp, PartialVerificationError, Option[PartialVerificationError])], havocHeap: Boolean = true,
                      isAssert: Boolean = false , statesStackForPackageStmt: List[Any] = null, insidePackageStmt: Boolean = false): (Stmt, ExhaleProofHint) = {

    nestedExhaleId += 1

    // create a definedness state that matches the state before the exhale
    val wellDefState = stateModule.freshTempStateKeepCurrent(s"ExhaleWellDef${nestedExhaleId - 1}")
    val (wellDefStateInitStmt, wellDefStateInitHint) = stateModule.initToCurrentStmt(wellDefState)

    // creating a new temp state if we are inside a package statement
    val curState = stateModule.state
    var initStmtWand: Stmt = Statements.EmptyStmt
    if(insidePackageStmt){
      val StateSetup(tempState, initStmt) = wandModule.createAndSetState(None)
      wandModule.tempCurState = tempState
      initStmtWand = initStmt
    }
    val tempState: StateRep = wandModule.tempCurState.asInstanceOf[StateRep]

    val (exhaleStmt, bodyHints) =
      (exps map (e =>
        {
          val defCheckData =
                DefinednessCheckData(
                  e._3,
                  Some(DefinednessState(() => stateModule.replaceState(wellDefState)))
                )

          val (stmt, hint) = exhaleConnective(e._1.whenExhaling, e._2, defCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert = isAssert, currentStateForPackage = tempState)
          (stmt, (hint, e._3.isDefined))
        }
      )).unzip

    val assumptions = MaybeCommentBlock("Free assumptions (exhale module)",
      exps map (e => allFreeAssumptions(e._1)))

    stateModule.replaceState(curState)

    nestedExhaleId -= 1

    //only emit initialization of the well-definedness state if the exhale is non-empty
    val wellDefStateInitStmtOpt : Stmt = if(exhaleStmt.flatten.isEmpty) { Nil } else { wellDefStateInitStmt }

    if ((exps map (_._1.isPure) forall identity) || !havocHeap || isAssert) {
      // if all expressions are pure, then there is no need for heap copies
      // if this is a translation of an Assert statement, there is also no need for heap copies
      val hint = DefaultExhaleProofHint(bodyHints, wellDefStateInitHint, None)
      (wellDefStateInitStmtOpt ++ initStmtWand ++ exhaleStmt ++ assumptions, hint)
    } else {
      val (endExhaleStmt, exhaleHeapVar) = endExhale
      val fullStmt =
        beginExhale ++
        wellDefStateInitStmtOpt ++
        initStmtWand ++
          exhaleStmt ++
          assumptions ++
          Comment("Finish exhale") ++
          endExhaleStmt

      val hint = DefaultExhaleProofHint(bodyHints, wellDefStateInitHint, Some(exhaleHeapVar))
      (fullStmt, hint)
    }

  }

  /**
    * Emits a definedness check for the input expression if `defCheckData` demands a check and otherwise returns the
    * empty statement
    */
  private def maybeDefCheck(e: sil.Exp, defCheckData: DefinednessCheckData): Stmt = {
    defCheckData.performDefinednessChecks.fold(Statements.EmptyStmt: Stmt)(definednessError => checkDefinedness(e, definednessError, makeChecks = true, Some(defCheckData.definednessStateOpt.get)))
  }


  /**
   * Exhales Viper expression connectives (such as logical and/implication) and forwards the
   * translation of other expressions to the exhale components.
    * @param currentStateForPackage The temporary state connected to current statement being evaluated inside package statement
    *                  Access to the current state is needed during translation of an exhale during packaging a wand
   */
  private def exhaleConnective(e: sil.Exp, error: PartialVerificationError, definednessCheckData: DefinednessCheckData,
                               havocHeap: Boolean = true, statesStackForPackageStmt: List[Any] = null, insidePackageStmt: Boolean = false,
                               isAssert: Boolean, currentStateForPackage: StateRep): (Stmt, ExhaleBodyProofHint) = {

    e match {
      case sil.And(e1, e2) =>
        val (stmt1, hint1) = exhaleConnective(e1, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
        val (stmt2, hint2) = exhaleConnective(e2, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
        (stmt1 :: stmt2 :: Nil, StarExhaleHint(hint1, hint2))
      case sil.Implies(e1, e2) =>
        val defCheck = maybeDefCheck(e1, definednessCheckData)
        val (exhaleTranslation, exhaleHint): (Stmt, ImpExhaleHint) =
          if (insidePackageStmt) {
            val lhsTranslation = wandModule.getCurOpsBoolvar() ==> translateExpInWand(e1)
            val (rhsTranslation, rhsHint) = exhaleConnective(e2, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
            val res = If(lhsTranslation, rhsTranslation, Statements.EmptyStmt)
            val unionStmt = wandModule.updateUnion()
            (res ++ unionStmt, ImpExhaleHint(e1, rhsHint))
          } else {
            val lhsTranslation = translateExpInWand(e1)
            val (rhsTranslation, rhsHint) = exhaleConnective(e2, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
            (If(lhsTranslation, rhsTranslation, Statements.EmptyStmt), ImpExhaleHint(e1, rhsHint))
          }
        (defCheck ++ exhaleTranslation, exhaleHint)
      case sil.CondExp(c, e1, e2) =>
        val defCheck = maybeDefCheck(c, definednessCheckData)
        val (exhaleTranslation, exhaleHint) : (Stmt, CondExhaleHint) =
          if(insidePackageStmt) {
            val curOpsBoolVar = wandModule.getCurOpsBoolvar()
            val condTranslation = translateExpInWand(c)
            val (thnTranslation, thnHint) = exhaleConnective(e1, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
            val (elsTranslation, elsHint) = exhaleConnective(e2, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)

              // The union state should be updated because the union state calculated inside exhaleExt (let's call it state U') is inside the then part of if(c){}
              // and outside the if condition c is not satisfied we still evaluate expressions and check definedness in U' without knowing any assumption about it
            (
              If(curOpsBoolVar, If(condTranslation, thnTranslation, elsTranslation), Statements.EmptyStmt) ++ wandModule.updateUnion(),
              CondExhaleHint(c, thnHint, elsHint)
            )
          } else {
            val condTranslation = translateExpInWand(c)
            val (thnTranslation, thnHint) = exhaleConnective(e1, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
            val (elsTranslation, elsHint) = exhaleConnective(e2, error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)
            (If(condTranslation, thnTranslation, elsTranslation), CondExhaleHint(c, thnHint, elsHint))
          }
        (defCheck ++ exhaleTranslation, exhaleHint)
      case sil.Let(declared,boundTo,body) if !body.isPure =>
      {
        val defCheck = maybeDefCheck(boundTo, definednessCheckData)
        val u = env.makeUniquelyNamed(declared) // choose a fresh binder
        env.define(u.localVar)
        val resStmt =
          defCheck ::
          Assign(translateLocalVar(u.localVar),translateExpInWand(boundTo)) ::
            exhaleConnective(body.replace(declared.localVar, u.localVar), error, definednessCheckData, havocHeap, statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)._1 ::
            {
              env.undefine(u.localVar)
              Nil
            }
        (resStmt, NotSupportedExhaleHint)
      }
      case fa@sil.Forall(vars, _, body) if fa.isPure =>
        /* We use a special case for pure quantifiers, because the standard approach of just asserting the pure quantifier
           cannot use the information gained from unfoldings inside the quantifier body (which may be required to
           prove the quantifier). Instead, we invoke exhaleConnective on the quantifier body, which gives exhale components the
           option to gain more information from unfoldings, which can then be used when asserting parts of the quantifier body.
         */
      {
        //We do the definedness check separately
        val defCheck = maybeDefCheck(fa, definednessCheckData)

        val varsFresh = vars.map(v => env.makeUniquelyNamed(v))
        varsFresh.foreach(vFresh => env.define(vFresh.localVar))

        val renamedBody = Expressions.instantiateVariables(body, vars.map(_.localVar), varsFresh.map(_.localVar))

        /* We switch off the definedness checks here, because we do it separately. It may be correct, to not do the
           definedness check separately and instead keep the definedness checks switched on.
         */
        val (exhaleBodyStmt, bodyHint)= exhaleConnective(renamedBody, error, definednessCheckData.copyWhereNoChecksPerformed, havocHeap, statesStackForPackageStmt,
          insidePackageStmt, isAssert, currentStateForPackage = currentStateForPackage)

        val resStmt =
          defCheck ++
          Seqn(Seq (
            NondetIf(Seqn(Seq(exhaleBodyStmt, Assume(FalseLit())))),
            //in the non-deterministic branch we checked the assertion, hence we assume the assertion in the main branch
            Assume(translateExp(e)),
            {
              varsFresh.foreach(vFresh => env.undefine(vFresh.localVar))
              Nil
            }
          ))

        (resStmt, ForallExhaleHint(fa, bodyHint))
      }
      case u@sil.Unfolding(_, body) if !insidePackageStmt =>
        /** We handle unfolding separately here so that exhale components have the option to gain more information by
          * executing the unfolding (and potentially other unfoldings inside the unfolding body) */
        val defCheck = maybeDefCheck(e, definednessCheckData)

        val definednessCheckDataRec =
          if(definednessCheckData.performDefinednessChecks.isDefined) {
            /**
              * We switch off the definedness checks here, because we do them separately in this case.
              * Moreover, we remove the information on the definedness state to avoid exhale components doing the same work
              * that the definedness components do. This can lead to incompletenesses in cases where the information
              * gained by the definedness components is not available in the exhale context (should not happen frequently in practice).
              */
            DefinednessCheckData(None, None)
          } else {
            definednessCheckData
          }

        val checksAndHints = components map (_.exhaleExpBeforeAfter(e, error, definednessCheckDataRec.definednessStateOpt))
        val (stmtBefore, hintsBefore) = (checksAndHints map (_._1())).unzip

        val (exhaleBody, bodyHint) = exhaleConnective(body, error, definednessCheckDataRec, havocHeap,
          statesStackForPackageStmt, insidePackageStmt, isAssert, currentStateForPackage)

        val (stmtAfter, hintsAfter) = (checksAndHints map (_._2())).unzip

        (defCheck ++ stmtBefore ++ exhaleBody ++ stmtAfter, UnfoldingExhaleHint(u, hintsBefore.flatten, bodyHint, hintsAfter.flatten))
      case _ => {
        val defCheck = maybeDefCheck(e, definednessCheckData)

        if(insidePackageStmt) {  // handling exhales during packaging a wand
          // currently having wild cards and 'constraining' expressions are not supported during packaging a wand.
          if(!permModule.getCurrentAbstractReads().isEmpty) {
            sys.error("Abstract reads cannot be used during packaging a wand.")  // AG: To be changed to unsupportedFeatureException
          }

          if(permModule.containsWildCard(e)) {
            sys.error("Wild cards cannot be used during packaging a wand.") // AG: To be changed to unsupportedFeatureException
          }

          val exhaleExtStmt = wandModule.exhaleExt(statesStackForPackageStmt, currentStateForPackage, e, wandModule.getCurOpsBoolvar(), error = error, havocHeap = havocHeap)  // executing the exhale statement
          val addAssumptions = (statesStackForPackageStmt(0).asInstanceOf[StateRep].boolVar := statesStackForPackageStmt(0).asInstanceOf[StateRep].boolVar && currentStateForPackage.boolVar)  // adding needed assumptions to states boolean variables
          val equateHps = wandModule.exchangeAssumesWithBoolean(equateHeaps(statesStackForPackageStmt(0).asInstanceOf[StateRep].state, heapModule), statesStackForPackageStmt(0).asInstanceOf[StateRep].boolVar)
          val assertTransfer: Stmt =  // if it is an assert statement we transfer the permissions from temp state to ops state
            if(isAssert && !e.isPure) {
              val curState = stateModule.state
              stateModule.replaceState(statesStackForPackageStmt(0).asInstanceOf[StateRep].state)
              val stmt: Stmt = wandModule.exhaleExt(currentStateForPackage :: Nil, statesStackForPackageStmt(0).asInstanceOf[StateRep], e, wandModule.getCurOpsBoolvar(), error = error)
              stateModule.replaceState(curState)
              stmt
            }
            else
              Nil
          if (!havocHeap)
            (defCheck ++ exhaleExtStmt ++ addAssumptions ++ equateHps ++ assertTransfer, NotSupportedExhaleHint)
          else
            (defCheck ++ exhaleExtStmt ++ addAssumptions ++ assertTransfer, NotSupportedExhaleHint)
        } else {
          /* We propagate the definedness state to the components only if no definedness check was performed to avoid
            exhale components doing the same work as the definedness components do */
          val definednessCheckDataRec =
            if(definednessCheckData.performDefinednessChecks.isDefined) {
              None
            } else {
              definednessCheckData.definednessStateOpt
            }

          val (exhaleStmt, hintsBefore, hintsAfter) = invokeExhaleOnComponents(e, error, definednessCheckDataRec)

          val exhaleHint =
            e match {
              case _ if e.isPure => PureExpExhaleHint(e, hintsBefore.flatten, hintsAfter.flatten)
              case  acc : sil.FieldAccessPredicate => FieldAccessPredicateExhaleHint(acc, hintsBefore.flatten, hintsAfter.flatten)
              case _ => NotSupportedExhaleHint
            }

          (defCheck ++ exhaleStmt, exhaleHint)
        }
      }
    }
  }

  private def invokeExhaleOnComponents(e: sil.Exp, error: PartialVerificationError, definednessCheckOpt: Option[DefinednessState]) : (Stmt, Seq[ExhaleComponentProofHint], Seq[ExhaleComponentProofHint]) = {
    val checks = components map (_.exhaleExpBeforeAfter(e, error, definednessCheckOpt))
    val (stmtBefore, hintsBefore) = (checks map (_._1())).unzip
    // some implementations may rely on the order of these calls

    val (stmtAfter, hintsAfter) = (checks map (_._2())).unzip

    (stmtBefore ++ stmtAfter, hintsBefore.flatten, hintsAfter.flatten)
  }

  /**
   * Handles only pure expressions - others will be dealt with by other modules
   */
  override def exhaleExp(e: sil.Exp, error: PartialVerificationError, definednessCheckIncluded: Option[DefinednessState]): (Stmt, Seq[ExhaleComponentProofHint]) =
  {
    if (e.isPure) {
      e match {
        case _ : sil.Unfolding => (Nil, Seq()) //handled by exhaleConnective
        case _ => (Assert(translateExp(e), error.dueTo(AssertionFalse(e))), Seq())
      }
    } else {
      (Nil, Seq())
    }
  }
}


/**
  * Class to specify definedness check information during exhale
  *
  * @param performDefinednessChecks empty if no definedness check must be performed, else contains the corresponding definedness error
  * @param definednessStateOpt Contains the state in which definedness checks should be performed. Must be non-empty if definedness checks
  *                         must be performed. If definedness checks should not be performed, then the definedness state can be used to
  *                         gain additional information. In the latter case, this field may be empty when, for example, the definedness state
  *                         is not tracked.
  */
case class DefinednessCheckData(performDefinednessChecks: Option[PartialVerificationError], definednessStateOpt: Option[DefinednessState])
{

  def copyWhereNoChecksPerformed : DefinednessCheckData  = this.copy(performDefinednessChecks = None)

}