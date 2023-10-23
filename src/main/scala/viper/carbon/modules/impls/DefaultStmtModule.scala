// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules.{StatelessComponent, StmtModule}
import viper.carbon.modules.components.{DefinednessComponent, DefinednessState, SimpleStmtComponent}
import viper.silver.ast.utility.Expressions.{whenExhaling, whenInhaling}
import viper.silver.{ast => sil}
import viper.carbon.boogie._
import viper.carbon.verifier.Verifier
import Implicits._
import viper.carbon.proofgen.hints.{AssertStmtComponentHint, AssertStmtHint, AtomicHint, ExhaleStmtComponentHint, ExhaleStmtHint, FieldAssignHint, IfComponentHint, IfHint, InhaleStmtComponentHint, InhaleStmtHint, LocalVarAssignHint, MethodCallHint, MethodCallStmtComponentHint, ScopeProofHint, SeqnProofHint, StmtComponentProofHint, StmtProofHint}
import viper.silver.ast.FieldAssign
import viper.silver.verifier.{PartialVerificationError, errors, reasons}
import viper.silver.ast.utility.Expressions

/**
 * The default implementation of a [[viper.carbon.modules.StmtModule]].
 */
class DefaultStmtModule(val verifier: Verifier) extends StmtModule with SimpleStmtComponent with StatelessComponent with DefinednessComponent {

  import verifier._
  import expModule._
  import stateModule._
  import exhaleModule._
  import inhaleModule._
  import funcPredModule._
  import wandModule._

  override def start(): Unit = {
    // this is the main translation, so it should come at the "beginning"; it defines the innermost code used in the translation; other modules can wrap this with their own code
    register(this, before = Seq(verifier.heapModule,verifier.permModule))
    // NOTE: this builds up the translation inside-out, so the *first* component defines the innermost code.
    // This works as follows, for statement translation: StmtModule, then PermModule, then HeapModule
    // For Fold statements: Heap module adds version/secondary mask code as a postfix to the main code from the StmtModule
    // For Field assignments: Heap module (which goes last) adds the translation of the actual operation as a postfix the other code (which checks well-definedness)
    // For MethodCall: assumptions about return values are added by the HeapModule as a postfix to the main translation in StmtModule
    // For New: the operation translation (HeapModule) is added as a prefix to the code adding permissions (PermModule)
    expModule.register(this)
  }

  private val lblNamespace = verifier.freshNamespace("stmt.lbl")
  var lblVarsNamespace = verifier.freshNamespace("var.lbl")
  override def labelNamespace = lblNamespace

  def name = "Statement module"

  /**
    * For each label we track a boolean that indicates whether the label has been defined in the trace
    */
  var labelBooleanGuards : collection.mutable.Map[String, LocalVarDecl] = new collection.mutable.HashMap[String, LocalVarDecl]()

  override def initStmt(methodBody: sil.Stmt): Stmt = {
    labelBooleanGuards = new collection.mutable.HashMap[String, LocalVarDecl]()

    //create a boolean variable declaration for each label
    methodBody.visit(
      n => n match {
        case sil.Label(name,_) =>
          labelBooleanGuards.put(name, LocalVarDecl(Identifier(name+"_lblGuard")(lblVarsNamespace), Bool))
      }
    )

    for (boolDecls <- labelBooleanGuards.values.toList) yield {
      boolDecls.l := FalseLit()
    }
  }

  /**
    * Takes a list of assertions, and executes all of the unfolding expressions inside. In particular, this means that the correct
   * predicate definitions get assumed, under the correct conditionals. Most checking of definedness is disabled (by the "false"
   * parameter to checkDefinedness), however, note that checking that the predicates themselves are held when unfolding is still
   * performed, because the code for that is an exhale. Because of this, it may be necessary to make sure that this operation is
   * called before/after the corresponding exhale/inhale of the assertions.
   */
  def executeUnfoldings(exps: Seq[sil.Exp], exp_error: (sil.Exp => PartialVerificationError)): Stmt = {
    (exps map (exp => (if (exp.existsDefined[Unit]({case sil.Unfolding(_,_) => })) checkDefinedness(exp, exp_error(exp), false) else Nil:Stmt)))
  }

  /**
    * @param statesStackForPackageStmt stack of states used in translating package statements
    * @param insidePackageStmt Boolean that represents whether 'stmt' is being translated inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def handleStmt(s: sil.Stmt, statesStackForPackageStmt: List[Any] = null, allStateAssms: Exp = TrueLit(), insidePackageStmt: Boolean = false) : (Seqn, Seq[StmtComponentProofHint]) => (Seqn, Seq[StmtComponentProofHint]) = {
    val ((bef, aft), sHints) =
      s match {
        case s: sil.Fold => (translateFold(s, statesStackForPackageStmt, insidePackageStmt), Seq()) //once fold produces hints need to also adjust hint ordering below
        case _ => {
          val (simpleStmts, simpleProofHints) = simpleHandleStmt(s, statesStackForPackageStmt, allStateAssms, insidePackageStmt)
          ((simpleStmts, Statements.EmptyStmt), simpleProofHints)
        } // put code *first*
      }
    {
      case (stmts, hints) => (bef ++ stmts ++ aft, sHints ++ hints)
    }
  }

  /**
    * @param statesStack stack of states used in translating statements during packaging a wand (carries currentState and LHS of wands)
    * @param insidePackageStmt Boolean that represents whether 'stmt' is being translated inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def simpleHandleStmt(stmt: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Exp = TrueLit(), insidePackageStmt: Boolean = false): (Stmt, Seq[StmtComponentProofHint]) = {
    if(loopModule.isLoopDummyStmt(stmt)) {
      //statement was just added for loop information purposes (only loopModule cares about it)
      return (Nil, Seq())
    }

    //In certain cases, definedness checks should not be included inside a package statement
    def maybeDefError(error: PartialVerificationError) : Option[PartialVerificationError] = {
      if(insidePackageStmt) { None } else { Some(error) }
    }

    stmt match {
      case assign@sil.LocalVarAssign(lhs, rhs) =>
        (
          checkDefinedness(lhs, errors.AssignmentFailed(assign), insidePackageStmt = insidePackageStmt) ++
            checkDefinedness(rhs, errors.AssignmentFailed(assign), insidePackageStmt = insidePackageStmt) ++
          {if(insidePackageStmt)
            Assign(translateExpInWand(lhs), translateExpInWand(rhs))
          else
            Assign(translateExp(lhs), translateExp(rhs))}
          , Seq()
        )
      case assign@sil.FieldAssign(lhs, rhs) =>
        (checkDefinedness(lhs.rcv, errors.AssignmentFailed(assign)) ++
          checkDefinedness(rhs, errors.AssignmentFailed(assign)), Seq())
      case fold@sil.Fold(e) => sys.error("Internal error: translation of fold statement cannot be handled by simpleHandleStmt code; found:" + fold.toString())
      case unfold@sil.Unfold(e) =>
        (translateUnfold(unfold, statesStack, insidePackageStmt), Seq())
      case inh@sil.Inhale(e) =>
        val (stmt, inhaleHint) = inhaleWithDefinednessCheck(whenInhaling(e), errors.InhaleFailed(inh), statesStack, insidePackageStmt)
        (stmt, Seq(InhaleStmtComponentHint(inhaleHint)))
      case exh@sil.Exhale(e) =>
        val transformedExp = whenExhaling(e)
        val defErrorOpt = maybeDefError(errors.ExhaleFailed(exh))
        val (stmt, exhaleHint) = exhale(Seq((transformedExp, errors.ExhaleFailed(exh), defErrorOpt)), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt)
        (stmt, Seq(ExhaleStmtComponentHint(exhaleHint)))
      case a@sil.Assert(e) =>
        val transformedExp = whenExhaling(e)
        val defErrorOpt = maybeDefError(errors.AssertFailed(a))

        if (transformedExp.isPure) {
          // if e is pure, then assert and exhale are the same
          val (exhaleStmt, exhaleHint) = exhale(Seq((transformedExp, errors.AssertFailed(a), defErrorOpt)), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt)
          (exhaleStmt, AssertStmtComponentHint(None, exhaleHint))
        } else {
          // we create a temporary state to ignore the side-effects
          val ((backup, snapshot), setupAssertState) = freshTempState("Assert")
          val (exhaleStmt, exhaleHint) = exhale(Seq((transformedExp, errors.AssertFailed(a), defErrorOpt)), isAssert =  true, statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt, havocHeap = false)
          replaceState(snapshot)
          (backup :: exhaleStmt :: Nil, AssertStmtComponentHint(Some(setupAssertState), exhaleHint))
        }
      case mc@sil.MethodCall(methodName, args, targets) =>
        val method = verifier.program.findMethod(methodName)
        // save pre-call state
        val (preCallStateStmt, state) = stateModule.freshTempState("PreCall")._1
        val preCallState = stateModule.state
        val oldState = stateModule.oldState
        stateModule.replaceState(state)
        val toUndefine = collection.mutable.ListBuffer[sil.LocalVar]()
        val actualArgs = args.zipWithIndex map (a => {
          val (actual, i) = a
          // use the concrete argument if it is just a variable or constant (to avoid code bloat)
          val useConcrete = actual match {
            case v: sil.LocalVar if !targets.contains(v) => true
            case _: sil.Literal => true
            case _ => false
          }
          if (!useConcrete) {
            val silFormal = method.formalArgs(i)
            val tempArg = sil.LocalVar("arg_" + silFormal.name, silFormal.typ)()
            mainModule.env.define(tempArg)
            toUndefine.append(tempArg)
            val translatedTempArg = translateExp(tempArg)
            val translatedActual = translateExp(actual)
            val stmt = translatedTempArg := translatedActual
            (tempArg, stmt, Some(actual))
          } else {
            (args(i), Nil: Stmt, None)
          }
        })
        val neededRenamings : Seq[(sil.AbstractLocalVar, sil.Exp)] = actualArgs.filter((_._3.isDefined)).map(element => (element._1.asInstanceOf[sil.LocalVar],element._3.get))
        val removingTriggers: (errors.ErrorNode => errors.ErrorNode) =
          ((n: errors.ErrorNode) => n.transform{case q: sil.Forall => q.copy(triggers = Nil)(q.pos, q.info, q.errT)})
        val renamingArguments : (errors.ErrorNode => errors.ErrorNode) = ((n:errors.ErrorNode) => removingTriggers(n).transform({
          case e:sil.Exp => Expressions.instantiateVariables[sil.Exp](e,neededRenamings map (_._1), neededRenamings map (_._2))
        }))

        val pres = method.pres map (e => Expressions.instantiateVariables(e, method.formalArgs ++ method.formalReturns, (actualArgs map (_._1)) ++ targets, mainModule.env.allDefinedNames(program)))
        val posts = method.posts map (e => Expressions.instantiateVariables(e, method.formalArgs ++ method.formalReturns, (actualArgs map (_._1)) ++ targets, mainModule.env.allDefinedNames(program)))
        val (exhalePreStmt, exhalePreHint) = exhaleWithoutDefinedness(pres map (e => (e, errors.PreconditionInCallFalse(mc).withReasonNodeTransformed(renamingArguments))), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt)
        val translatedTargets = (targets map translateExp).asInstanceOf[Seq[Var]]
        val (inhalePostStmt, inhalePostHint) = inhale(posts map (e => (e, errors.CallFailed(mc).withReasonNodeTransformed(renamingArguments))), addDefinednessChecks = false, statesStack, insidePackageStmt)

        val res =
          (if(!verifier.generateProofs) preCallStateStmt else Statements.EmptyStmt) ++ //TODO: support old state in proofgen
          (targets map (e => checkDefinedness(e, errors.CallFailed(mc), insidePackageStmt = insidePackageStmt))) ++
          (args map (e => checkDefinedness(e, errors.CallFailed(mc), insidePackageStmt = insidePackageStmt))) ++
          (actualArgs map (_._2)) ++
          MaybeCommentBlock("Exhaling precondition", executeUnfoldings(pres, (pre => errors.PreconditionInCallFalse(mc).withReasonNodeTransformed(renamingArguments))) ++ exhalePreStmt) ++
          MaybeCommentBlock("Havocing target variables", Havoc(translatedTargets)) ++ //CARBON_CHANGE: havoc between exhale and inhale instead of before exhale, since it is the more natural translation
          {
            stateModule.replaceOldState(preCallState)
            val res = MaybeCommentBlock("Inhaling postcondition",
              inhalePostStmt ++
              executeUnfoldings(posts, (post => errors.Internal(post).withReasonNodeTransformed(renamingArguments))))
            stateModule.replaceOldState(oldState)
            toUndefine map mainModule.env.undefine
            res
          }
        (res, Seq(MethodCallStmtComponentHint(methodName, translatedTargets, exhalePreHint.conjoinBodyHints, inhalePostHint.conjoinBodyHints)))
      case sil.While(_, _, _) =>
        //handled by LoopModule
        (Nil, Seq())
      case i@sil.If(cond, thn, els) =>
        val defCheck = checkDefinedness(cond, errors.IfFailed(cond), insidePackageStmt = insidePackageStmt)
        //TODO proof_gen: merge this change into Carbon master (makes proof easier)
        val condTr = if(allStateAssms == TrueLit()) { translateExpInWand(cond) } else { (allStateAssms) ==> translateExpInWand(cond) }
        val (thnTr, thnProofHints) = translateStmt(thn, statesStack, allStateAssms, insidePackageStmt)
        val (elsTr, elsProofHints) = translateStmt(els, statesStack, allStateAssms, insidePackageStmt)
        (
          defCheck ++
          If(condTr, thnTr, elsTr),
          Seq(IfComponentHint(thnProofHints, elsProofHints))
        )
      case sil.Label(name, _) => {
        val labelState = LabelHelper.getLabelState[stateModule.StateSnapshot](
          name,
          stateModule.freshTempStateKeepCurrent,
          stateModule.stateRepositoryGet, stateModule.stateRepositoryPut)
        //first label, then init statement: otherwise gotos to this label will skip the initialization
        (
        Label(Lbl(Identifier(name)(lblNamespace))) ++
          stateModule.initToCurrentStmt(labelState)._1 ++
          labelBooleanGuards.get(name).fold[Stmt](Nil)(labelGuardDecl => Seq(labelGuardDecl.l := TrueLit()))  //label is defined
        , Seq()
        )
      }
      case sil.Goto(_) =>
        /* Handled by loop module, since the loop module decides whether the goto should be translated as a goto. */
        (Nil, Seq())
      case pa@sil.Package(wand, proof) => {
        checkDefinedness(wand, errors.MagicWandNotWellformed(wand), insidePackageStmt = insidePackageStmt)
        (translatePackage(pa, errors.PackageFailed(pa), statesStack, allStateAssms, insidePackageStmt), Seq())
      }
      case a@sil.Apply(wand) =>
        (translateApply(a, errors.ApplyFailed(a), statesStack, allStateAssms, insidePackageStmt), Seq())
      case _ =>
        (Nil, Seq())
    }
  }

  /**
    * @param statesStack   stack of states used in translating package statements (carries currentState and LHS of wands)
    * @param duringPackage Boolean that represents whether this exhale is inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def translateStmt(stmt: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Exp = TrueLit(), duringPackage: Boolean = false): (Stmt, StmtProofHint) = {
    if(duringPackage) {
        wandModule.translatingStmtsInWandInit()
    }

    var comment = "Translating statement: " + stmt.toString.replace("\n", "\n  // ")
    stmt match {
      case sil.Seqn(ss, scopedDecls) =>
        val locals = scopedDecls.collect {case l: sil.LocalVarDecl => l}
        locals map (v => mainModule.env.define(v.localVar)) // add local variables to environment
        val s =
          {
            //CARBON_CHANGE: havoc of scoped variables should be merged into main Carbon repo
            val havocLocalsVars = MaybeCommentBlock("Havoc scoped variables", locals map (lv => Havoc(mainModule.env.get(lv.localVar))))
            val assmsLocalVars =
              MaybeCommentBlock("Assumptions about local variables", locals map (a => mainModule.allAssumptionsAboutValue(a.typ, mainModule.translateLocalVarDecl(a), true)))
            val translatedStmtsAndProofHints = ss map (st => translateStmt(st, statesStack, allStateAssms, duringPackage))
            val (translatedStmts, translatedProofs) = translatedStmtsAndProofHints.unzip

            /**
              * in Isabelle we represent Seqn( Seq(s) ) as s
              */
            val seqnProofHint = if(ss.size == 1) { translatedProofs(0) } else { SeqnProofHint(translatedProofs) }
            val scopeWithSeqnProofHint =
              if(scopedDecls.isEmpty) {
                seqnProofHint
              } else {
                ScopeProofHint(scopedDecls, locals map (v => mainModule.env.get(v.localVar)), seqnProofHint)
              }

            (Seqn(havocLocalsVars++assmsLocalVars++translatedStmts), scopeWithSeqnProofHint)
          }
        locals map (v => mainModule.env.undefine(v.localVar)) // remove local variables from environment
        // return to avoid adding a comment, and to avoid the extra 'assumeGoodState'
        return s
      case sil.If(cond, thn, els) =>
        comment = s"Translating statement: if ($cond)"
      case sil.While(cond, invs, body) =>
        comment = s"Translating statement: while ($cond)"
      case _ =>
    }

    var (stmts, proofHints) : (Seqn, Seq[StmtComponentProofHint]) = (Seqn(Nil), Seq())
    for (c <- components) { // NOTE: this builds up the translation inside-out, so the *first* component defines the innermost code.
      //      val (before, after) = c.handleStmt(stmt, statesStack, allStateAssms, inWand)
      //      stmts = before ++ stmts ++ after
      val f = c.handleStmt(stmt, statesStack, allStateAssms, duringPackage)
      val (stmtsNew,proofHintsNew) = f(stmts, proofHints)
      stmts = stmtsNew
      proofHints = proofHintsNew
    }
    if (stmts.children.size == 0 && !loopModule.isLoopDummyStmt(stmt)) {
      assert(assertion = false, "Translation of " + stmt + " is not defined")
    }
    val translation = stmts ::
      (if(duringPackage){
        exchangeAssumesWithBoolean(assumeGoodState, statesStack.head.asInstanceOf[StateRep].boolVar)
      }else{
        assumeGoodState
      }) ::
      Nil

    (CommentBlock(comment + s" -- ${stmt.pos}", translation), proofHint(stmt, proofHints))
  }

  private def proofHint(s: sil.Stmt, proofHints: Seq[StmtComponentProofHint]) : StmtProofHint = {
    s match {
      case sil.NewStmt(lhs, fields) => ???
      case assign@sil.LocalVarAssign(lhs, rhs) => AtomicHint(LocalVarAssignHint(assign, mainModule.env.get(lhs), proofHints))
      case fa@sil.FieldAssign(lhs, rhs) => AtomicHint(FieldAssignHint(fa, proofHints))
      case sil.MethodCall(methodName, args, targets) => AtomicHint(MethodCallHint(proofHints))
      case sil.Exhale(exp) => AtomicHint(ExhaleStmtHint(proofHints))
      case sil.Inhale(exp) => AtomicHint(InhaleStmtHint(proofHints))
      case sil.Assert(exp) => AtomicHint(AssertStmtHint(proofHints))
      case sil.Assume(exp) => ???
      case sil.Fold(acc) => ???
      case sil.Unfold(acc) => ???
      case sil.Package(wand, proofScript) => ???
      case sil.Apply(exp) => ???
      case sil.Seqn(ss, scopedDecls) => ???
      case sil.If(cond, thn, els) => IfHint(cond, proofHints)
      case sil.While(cond, invs, body) => ???
      case sil.Label(name, invs) => ???
      case sil.Goto(target) => ???
      case sil.LocalVarDeclStmt(decl) => ???
      case _: sil.ExtensionStmt => ???
    }
  }


  override def simplePartialCheckDefinednessBefore(e: sil.Exp, error: PartialVerificationError, makeChecks: Boolean, definednessStateOpt: Option[DefinednessState]): Stmt = {
    if(makeChecks) {
      e match {
        case labelOld@sil.LabelledOld(_, labelName) =>
          labelBooleanGuards.get(labelName) match {
            case Some(labelGuardDecl) =>
              Assert(labelGuardDecl.l, error.dueTo(reasons.LabelledStateNotReached(labelOld)))
            case None => Nil
          }
        case _ => Nil
      }
    } else Nil
  }
}
