package viper.carbon.proofgen.hints

import isabelle.ast.{IsaUtil, MLUtil, ProofUtil, Term}
import viper.carbon.boogie.{Axiom, CommentedDecl, Decl, Func}
import viper.carbon.proofgen.BoogieFunId

sealed trait BoogieDeclProofHint

case class BoogieFuncProofHint(funcId: BoogieFunId, private val tactic: Seq[String]) extends BoogieDeclProofHint
{
  def funInterpWellFormednessProof() : Seq[String] = tactic

}

object BoogieFuncProofHint {

  def createStandardProofHint(funcId: BoogieFunId) : BoogieFuncProofHint = {
    val tactic =
      ProofUtil.applyTac(s"fun_interp_wf_aux_tac ${funcId.isaRepr} fun_wf_thm: ${funcId.funWfThm}")

    BoogieFuncProofHint(funcId, Seq(tactic))
  }

}

case class BoogieAxiomProofHint(axiomName: String, private val tactic: AxiomTacticInput => Seq[String]) extends BoogieDeclProofHint
{
  def satProof(axiomTacticInput: AxiomTacticInput) : Seq[String] = {
    tactic(axiomTacticInput)
  }

}

case class AxiomTacticInput(funInterpDef: String, boogieConstRel: String, fieldRel: String, lookupFieldLemmas: String, delThms: String)

object BoogieAxiomProofHint {

  def createStandardAxiomHint(axiomName: String, terminatingTacticWithoutApply: Seq[String]) : BoogieAxiomProofHint = {
    val tactic = (axiomTacticInput : AxiomTacticInput) => {
      val funInterpDefLemmaML = MLUtil.isaToMLThm(IsaUtil.definitionLemmaFromName(axiomTacticInput.funInterpDef))

      val lookupConstLemmas = MLUtil.isaToMLThms(ProofUtil.OF("lookup_boogie_const_concrete_lemmas", axiomTacticInput.boogieConstRel))
      val lookupFieldLemmas = MLUtil.isaToMLThms(ProofUtil.OF(axiomTacticInput.lookupFieldLemmas, axiomTacticInput.fieldRel))
      val axiomSatProofDel = MLUtil.isaToMLThms(axiomTacticInput.delThms)

      val initTacWithoutApply = Seq(
        "axiom_proof_init",
        ProofUtil.ruleTac("expr_sat_rewrite"),
        MLUtil.mlTacticToIsa(MLUtil.app("axiom_tac", Seq(MLUtil.contextAniquotation, funInterpDefLemmaML, lookupConstLemmas, lookupFieldLemmas, axiomSatProofDel, "1")))
      )

      ProofUtil.mapApplyTac(initTacWithoutApply ++ terminatingTacticWithoutApply)
    }

    BoogieAxiomProofHint(axiomName, tactic)
  }

}

case object BoogieDeclNotSupportedHint extends BoogieDeclProofHint


object BoogieDeclProofHint {

  def extractHintsFromAllDecls(decl: Decl) : Seq[BoogieDeclProofHint] =
    extractHintsFromDecl(decl, true)

  def extractHintsFromDeclsThatHaveHints(decl: Decl) : Seq[BoogieDeclProofHint] =
    extractHintsFromDecl(decl, false)

  def extractHintsFromDecl(decl: Decl, throwExceptionIfHintMissing: Boolean) : Seq[BoogieDeclProofHint] = {
    def handleNoneCase(name: String) : Seq[BoogieDeclProofHint] =
      if(throwExceptionIfHintMissing) {
        throw new IllegalArgumentException(s"$name does not have a proof hint")
      } else {
        Seq()
      }

    decl match {
      case f: Func =>
        f.proofHint match {
          case None => handleNoneCase("function "+ f.name.name)
          case Some(h) => Seq(h)
        }
      case ax: Axiom =>
        ax.proofHint match {
          case None => handleNoneCase("axiom ")
          case Some(h) => Seq(h)
        }
      case c : CommentedDecl => c.d.flatMap( (d: Decl) => extractHintsFromDecl(d, throwExceptionIfHintMissing))
      case _ => Seq()
    }
  }

}