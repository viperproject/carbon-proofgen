package viper.carbon.proofgen.hints

import viper.carbon.proofgen.IsaMethodSpecificationHelper
import viper.silver.{ast => sil}

case class InhaleProofHint(bodyHint: InhaleBodyProofHint, addWelldefinednessChecks: Boolean)
/*
{

  def conjoinBodyHints : InhaleProofHint = {
    if(bodyHints.isEmpty) {
      this
    } else {
      InhaleProofHint(Seq(IsaMethodSpecificationHelper.conjoinSpecInhaleHints(bodyHints)), addWelldefinednessChecks)
    }
  }

}
 */

//object NotSupportedInhaleHint extends InhaleProofHint(bodyHints = Seq(), addWelldefinednessChecks = true)

object InhaleProofHint {

  def combineHints(hints: Seq[InhaleProofHint], addWelldefinednessChecks : Boolean) : InhaleProofHint = {
    InhaleProofHint(IsaMethodSpecificationHelper.conjoinSpecInhaleHints(hints.map(h => h.bodyHint)), addWelldefinednessChecks)
  }

  def combineBodyHintsIntoHint(hints: Seq[InhaleBodyProofHint], addWelldefinednessChecks: Boolean): InhaleProofHint = {
    InhaleProofHint(IsaMethodSpecificationHelper.conjoinSpecInhaleHints(hints), addWelldefinednessChecks)
  }

  def combineBodyHintsIntoBodyHint(hints: Seq[InhaleBodyProofHint]): InhaleBodyProofHint = {
    IsaMethodSpecificationHelper.conjoinSpecInhaleHints(hints)
  }

  //val empty = InhaleProofHint(Seq(), true)

}

/** Inhale */
sealed trait InhaleBodyProofHint
case class StarInhaleHint(left: InhaleBodyProofHint, right: InhaleBodyProofHint) extends InhaleBodyProofHint
case class ImpInhaleHint(cond: sil.Exp, right: InhaleBodyProofHint) extends InhaleBodyProofHint
case class CondInhaleHint(cond: sil.Exp, thn: InhaleBodyProofHint, els: InhaleBodyProofHint) extends InhaleBodyProofHint
case class GoodStateAfterInhaleHint(hint: InhaleBodyProofHint) extends InhaleBodyProofHint
case object TrivialInhaleHint extends InhaleBodyProofHint

sealed trait AtomicInhaleHint extends InhaleBodyProofHint
case class FieldAccessPredicateInhaleHint(fieldAccessPred: sil.FieldAccessPredicate, hints: Seq[InhaleComponentProofHint]) extends AtomicInhaleHint
case class PureExpInhaleHint(e: sil.Exp, hints: Seq[InhaleComponentProofHint]) extends AtomicInhaleHint
case object NotSupportedAtomicInhaleHint extends AtomicInhaleHint
