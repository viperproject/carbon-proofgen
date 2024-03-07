// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules._
import viper.silver.ast.utility.Expressions
import viper.silver.{ast => sil}
import viper.carbon.boogie._
import viper.carbon.boogie.Implicits._

import java.text.SimpleDateFormat
import java.util.Date
import viper.carbon.boogie.CommentedDecl
import viper.carbon.boogie.Procedure
import viper.carbon.boogie.Program
import viper.carbon.proofgen.hints.{InhaleBodyProofHint, InhaleProofHint, MethodProofHint, NotSupportedAtomicInhaleHint, NotSupportedInhaleHint, StateProofHint, StmtProofHint}
import viper.carbon.verifier.Environment
import viper.silver.verifier.{TypecheckerWarning, errors}
import viper.carbon.verifier.Verifier
import viper.silver.ast.Quasihavoc
import viper.silver.ast.utility.rewriter.Traverse
import viper.silver.reporter.{Reporter, WarningsDuringTypechecking}

import java.nio.file.Path
import scala.collection.mutable

/**
 * The default implementation of a [[viper.carbon.modules.MainModule]].
 */
class DefaultMainModule(val verifier: Verifier) extends MainModule with StatelessComponent {

  import verifier._
  import typeModule._
  import stmtModule._
  import exhaleModule._
  import heapModule._
  import funcPredModule._
  import domainModule._
  import expModule._

  def name = "Main module"

  override val silVarNamespace = verifier.freshNamespace("main.silver")
  implicit val mainNamespace = verifier.freshNamespace("main")

  var proofDir: Option[Path] = None

  override def translateLocalVarSig(typ:sil.Type, v:sil.LocalVar): LocalVarDecl = {
    val t: Type = translateType(typ)
    val name: Identifier = env.get(v).name
    LocalVarDecl(name, t)
  }

  override def translate(p: sil.Program, reporter: Reporter): (Program, Map[String, Map[String, String]]) = {

    verifier.replaceProgram(
      p.transform(
        {
          case f: sil.Forall => {
            val res = f.autoTrigger
            if (res.triggers.isEmpty) {
              reporter.report(WarningsDuringTypechecking(Seq(TypecheckerWarning("No triggers provided or inferred for quantifier.", res.pos))))
            }
            res
          }
          case e: sil.Exists => {
            val res = e.autoTrigger
            if (res.triggers.isEmpty) {
              reporter.report(WarningsDuringTypechecking(Seq(TypecheckerWarning("No triggers provided or inferred for quantifier.", res.pos))))
            }
            res
          }
          case q: Quasihavoc => desugarQuasihavoc(q)
        },
        Traverse.TopDown)
    )

    val backendFuncs = new mutable.HashSet[sil.DomainFunc]()
    for (d <- p.domains) {
      backendFuncs.addAll(d.functions.filter(f => f.interpretation.isDefined))
    }

    // We record the Boogie names of all Viper variables in this map.
    // The format is Viper member name -> (Viper variable name -> Boogie variable name).
    var nameMaps : Map[String, mutable.HashMap[String, String]] = null

    val output = verifier.program match {
      case sil.Program(domains, fields, functions, predicates, methods, extensions) =>
        // translate all members

        // important to convert Seq to List to force the methods to be translated, otherwise it's possible that
        // evaluation happens lazily, which can lead to incorrect behaviour (evaluation order is important here)
        val translateFields =
          MaybeCommentedDecl("Translation of all fields", (fields flatMap translateField).toList)
        nameMaps = (methods ++ functions ++ predicates).map(_.name -> new mutable.HashMap[String, String]()).toMap
        val members = (domains flatMap translateDomainDecl) ++
          translateFields ++
          (functions flatMap (f => translateFunction(f, nameMaps.get(f.name)))) ++
          (predicates flatMap (p => translatePredicate(p, nameMaps.get(p.name)))) ++
          (methods flatMap (m => translateMethodDecl(m, nameMaps.get(m.name)))) ++
          (backendFuncs flatMap translateBackendFunc)

        // get the preambles (only at the end, even if we add it at the beginning)
        val preambles = verifier.allModules flatMap {
          m =>
            if (m.preamble.size > 0) Seq(CommentedDecl(s"Preamble of ${m.name}.", m.preamble))
            else Nil
        }

        if(verifier.generateProofs) {
          //proofs for the preambles must be generated after the preambles are generated
          verifier.proofGenInterface.generateProofForPreamble(preambles ++ translateFields)

          //generate main end-to-end theorem (must happen after all proofs for the methods have been generated)
          verifier.proofGenInterface.generateEndToEndProof()
        }

        // some header information for debugging
        val deps = verifier.dependencyDescs map ("  " + _)
        val header = Seq(
          "",
          s"Translation of Viper program.",
          "",
          "Date:         " + new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()),
          "Tool:         " + verifier.toolDesc) ++
          (verifier.getDebugInfo map (a => a._1 + ": " + (" " * (12 - a._1.size)) + a._2)) ++
          Seq("Dependencies:") ++
          deps ++
          Seq("")
        Program(header, preambles ++ members)
    }

    if(generateProofs) {
      //TODO proof_gen: support optimization in proof generation
      (output, nameMaps.map(e => e._1 -> e._2.toMap))
    } else {
      (output.optimize.asInstanceOf[Program], nameMaps.map(e => e._1 -> e._2.toMap))
    }
  }

  def translateMethodDecl(m: sil.Method, names: Option[mutable.Map[String, String]]): Seq[Decl] = {
    val mWithLoopInfo = loopModule.initializeMethod(m)

    env = Environment(verifier, mWithLoopInfo)
    ErrorMemberMapping.currentMember = mWithLoopInfo
        val res = mWithLoopInfo match {
          case method @ sil.Method(name, formalArgs, formalReturns, pres, posts, _) =>
            val initOldStateComment = "Initializing of old state"
            val ins: Seq[LocalVarDecl] = formalArgs map translateLocalVarDecl
            val outs: Seq[LocalVarDecl] = formalReturns map translateLocalVarDecl
            val init = MaybeCommentBlock("Initializing the state", stateModule.initBoogieState ++ assumeAllFunctionDefinitions ++ stmtModule.initStmt(method.bodyOrAssumeFalse))
            val initOld = MaybeCommentBlock("Initializing the old state", stateModule.initOldState)
            val paramAssumptions = mWithLoopInfo.formalArgs map (a => allAssumptionsAboutValue(a.typ, translateLocalVarDecl(a), true))
            val (inhalePre, inhalePreHints) = translateMethodDeclPre(pres)
            val (checkPost: Stmt, postFramingHint) = if (posts.nonEmpty) {
              translateMethodDeclCheckPosts(posts)
            }
            else (Nil : Stmt, (Seq(), InhaleProofHint.empty))
            val postsWithErrors = posts map (p => (p, errors.PostconditionViolated(p, mWithLoopInfo)))
            val (exhalePost, exhalePostHint ) = exhaleWithoutDefinedness(postsWithErrors)
            val exhalePostWithComment = MaybeCommentBlock("Exhaling postcondition", exhalePost)
            val (body, bodyProofHint) : (Stmt, StmtProofHint) = translateStmt(method.bodyOrAssumeFalse)
              /* TODO: Might be worth special-casing on methods with empty bodies */
            val proc = Procedure(Identifier(name), ins, outs,
              Seq(init,
                MaybeCommentBlock("Assumptions about method arguments", paramAssumptions),
                inhalePre) ++
                (if(!generateProofs) {
                  Seq(MaybeCommentBlock(initOldStateComment, initOld))
                } else {
                  //TODO proof_gen: currently do not handle old expressions in proofs
                  Nil
                }) ++
                Seq(checkPost, body, exhalePostWithComment)
            )

            if(verifier.generateProofs) {
              val methodProofHint = MethodProofHint(inhalePreHints, postFramingHint, bodyProofHint, exhalePostHint)
              verifier.proofGenInterface.generateProofForMethod(m, proc, env, methodProofHint)
            }

        CommentedDecl(s"Translation of method $name", proc)
    }

    if (names.isDefined){
      val usedNames = env.currentNameMapping
      // add all local vars
      names.get ++= usedNames
    }

    env = null
    ErrorMemberMapping.currentMember = null
    res
  }

  private def translateMethodDeclCheckPosts(posts: Seq[sil.Exp]): (Stmt, (Seq[StateProofHint], InhaleProofHint)) = {
    val ((freshStateStmtAux, state), freshStateHint) = stateModule.freshTempState("Post", discardCurrent = true, initialise = true)
    val freshStateStmt = freshStateStmtAux ++ stateModule.assumeGoodState

    // note that the order here matters - onlyExhalePosts should be computed with respect to the reset state
    val (onlyExhalePosts: Seq[Stmt], inhaleProofHint) = inhaleModule.inhaleExhaleSpecWithDefinednessCheck(
    posts, {
      errors.ContractNotWellformed(_)
    })

    val (stmts, resultInhaleProofHint) = (
    if (Expressions.contains[sil.InhaleExhaleExp](posts)) {
      // Postcondition contains InhaleExhale expression.
      // Need to check inhale and exhale parts separately.
      val onlyInhalePosts: Seq[Stmt] = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
      posts, {
        errors.ContractNotWellformed(_)
      }).map(_._1)

      val resStmt =
          NondetIf(
            freshStateStmt ++
            MaybeComment("Checked inhaling of postcondition to check definedness",
              MaybeCommentBlock("Do welldefinedness check of the inhale part.",
                NondetIf(onlyInhalePosts ++ Assume(FalseLit()))) ++
                MaybeCommentBlock("Normally inhale the exhale part.",
                  onlyExhalePosts)
          ) ++
            MaybeComment("Stop execution", Assume(FalseLit()))
      )
      (resStmt, NotSupportedInhaleHint) //do not support inhale-exhale assertions for proof generation
    }
    else {
      val resStmt =
        NondetIf(
          freshStateStmt ++
          MaybeComment("Checked inhaling of postcondition to check definedness", onlyExhalePosts) ++
            MaybeComment("Stop execution", Assume(FalseLit()))
        )
      (resStmt, inhaleProofHint)
    })

    stateModule.replaceState(state)

    (stmts, (freshStateHint, resultInhaleProofHint))
  }

  private def translateMethodDeclPre(pres: Seq[sil.Exp]): (Stmt, InhaleProofHint) = {
    val res = if (Expressions.contains[sil.InhaleExhaleExp](pres)) {
      // Precondition contains InhaleExhale expression.
      // Need to check inhale and exhale parts separately.
      val onlyExhalePres: Seq[Stmt] = inhaleModule.inhaleExhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      }).map(_._1)
      val onlyInhalePres: Seq[Stmt] = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      }).map(_._1)
      (MaybeCommentBlock("Checked inhaling of precondition",
        MaybeCommentBlock("Do welldefinedness check of the exhale part.",
          NondetIf(onlyExhalePres ++ Assume(FalseLit()))) ++
          MaybeCommentBlock("Normally inhale the inhale part.",
            onlyInhalePres)
      ), InhaleProofHint.empty)
    }
    else {
      val (inhalePres, inhalePresHint) = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      })

      (MaybeCommentBlock("Checked inhaling of precondition", inhalePres), inhalePresHint)
    }

    res
  }

  override def allAssumptionsAboutValue(typ:sil.Type, arg: LocalVarDecl, isParameter:Boolean): Stmt = {
    val tmp = verifier.allModules map (_.validValue(typ, arg.l, isParameter))
    val assumptions = tmp.filter(_.isDefined).map(_.get)
    assumptions.allOption match {
      case None => Nil
      case Some(e) => Assume(e)
    }
  }

  def translateDomainDecl(d: sil.Domain): Seq[Decl] = {
    env = Environment(verifier, d)
    val res = translateDomain(d)
    env = null
    res
  }

  /***
    * Desugar a quasihavoc into an exhale followed by an inhale statement
    * @param q should be a field or pedicate quasihavoc
    * @return
    */
  private def desugarQuasihavoc(q: sil.Quasihavoc) = {
    val curPermVarDecl = sil.LocalVarDecl("perm_temp_quasihavoc_", sil.Perm)()
    val curPermVar = curPermVarDecl.localVar
    val resourceCurPerm =
      q.exp match {
        case r : sil.FieldAccess =>
          sil.FieldAccessPredicate(r, curPermVar)()
        case r: sil.PredicateAccess =>
          sil.PredicateAccessPredicate(r, curPermVar)()
        case _ => sys.error("Not supported resource in quasihavoc")
      }

    val curPermInhExPermission =
      sil.Seqn(
        sil.LocalVarAssign(curPermVar, sil.CurrentPerm(q.exp)())() +:
          Seq(
            sil.Exhale(resourceCurPerm)(),
            sil.Inhale(resourceCurPerm)()
          )
        ,
        Seq(curPermVarDecl)
      )()

    q.lhs match {
      case Some(cond) =>
        sil.If(cond,
          curPermInhExPermission,
          sil.Seqn(Seq(), Seq())()
        )()
      case None =>
        sil.Seqn(curPermInhExPermission, Seq())()
    }
  }
}
