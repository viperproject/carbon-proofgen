package viper.carbon.proofgen.hints

import viper.carbon.boogie.{CommentedDecl, Decl, Func}

sealed trait BoogieDeclProofHint

case class BoogieFuncProofHint(func: Func) extends BoogieDeclProofHint
{
  def funInterpWellFormednessProof() : Seq[String] = {
    ???
  }

}

case class BoogieAxiomProofHint(axiomTerminatingTactic: Seq[String]) extends BoogieDeclProofHint
{
  def satProof(funInterpDef: String, delThms: String) : Seq[String] = {
    ???
  }

}

case object BoogieDeclNotSupportedHint extends BoogieDeclProofHint


object BoogieDeclProofHint {

  def extractHintsFromDecl(decl: Decl) : Seq[BoogieDeclProofHint] = {
    decl match {
      case f: Func => f.proofHint.fold(Seq.empty[BoogieDeclProofHint])(a => Seq(a))
      case ax: Func => ax.proofHint.fold(Seq.empty[BoogieDeclProofHint])(a => Seq(a))
      case c : CommentedDecl => c.d.flatMap(extractHintsFromDecl)
      case _ => Seq()
    }
  }

}