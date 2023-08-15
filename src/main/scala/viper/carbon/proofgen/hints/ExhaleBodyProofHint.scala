package viper.carbon.proofgen.hints

import viper.carbon.boogie.LocalVar
import viper.carbon.proofgen.IsaMethodSpecificationHelper
import viper.silver.{ast => sil}

sealed trait ExhaleProofHint
{
  /***
    * if the hint represents a sequence of assertions, then the same hint should be returned where a single assertion
    * is represented that corresponds to the separating conjunction of the originally represented assertions
    * @return
    */
  def conjoinBodyHints  : ExhaleProofHint
}

/**
  *
  * @param bodyHint If the exhale method was invoked with a sequence of assertions (representing a separating conjunction),
  *                 then there is an element for each assertion, which consists of a hint [[ExhaleBodyProofHint]] and
  *                 a flag that is true iff well-definedness checks were performed in the exhale encoding.
  * @param setupWellDefStateHint hints to establish the well-definedness state for an exhale
  * @param exhaleHeapOpt if the heap is exhaled, then this parameter contains the local variable used in the Boogie encoding
  *                      for the temporary heap that is havoced and later used to update the current heap
  */
case class DefaultExhaleProofHint(bodyHint: Seq[(ExhaleBodyProofHint, Boolean)], setupWellDefStateHint: Seq[StateProofHint], exhaleHeapOpt: Option[LocalVar]) extends ExhaleProofHint
{

  override def conjoinBodyHints : ExhaleProofHint = {
    if(bodyHint.isEmpty) {
      this
    } else if(bodyHint.forall(h => h._2)) {
      DefaultExhaleProofHint(Seq((IsaMethodSpecificationHelper.conjoinSpecExhaleHints(bodyHint.map(h => h._1)), true)), setupWellDefStateHint, exhaleHeapOpt)
    } else if(bodyHint.forall(h => !h._2)) {
       DefaultExhaleProofHint(Seq((IsaMethodSpecificationHelper.conjoinSpecExhaleHints(bodyHint.map(h => h._1)), false)), setupWellDefStateHint, exhaleHeapOpt)
    } else {
      throw new IllegalArgumentException("exhale hint does not have a the same well-definedness check flag across assertions")
    }
  }

}


sealed trait ExhaleBodyProofHint

case class StarExhaleHint(left: ExhaleBodyProofHint, right: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class ImpExhaleHint(cond: sil.Exp, right: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class CondExhaleHint(cond: sil.Exp, thn: ExhaleBodyProofHint, els: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class ForallExhaleHint(f: sil.Forall, bodyHint: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class UnfoldingExhaleHint(u: sil.Unfolding, beforeUnfoldingHints: Seq[ExhaleComponentProofHint], bodyHint: ExhaleBodyProofHint, afterUnfoldingHints: Seq[ExhaleComponentProofHint]) extends ExhaleBodyProofHint
case object NotSupportedExhaleHint extends ExhaleBodyProofHint

sealed trait AtomicExhaleHint extends ExhaleBodyProofHint
case class FieldAccessPredicateExhaleHint(fieldAccessPred: sil.FieldAccessPredicate, hintsBefore: Seq[ExhaleComponentProofHint], hintsAfter: Seq[ExhaleComponentProofHint]) extends AtomicExhaleHint
case class PureExpExhaleHint(e: sil.Exp, beforeExhaleHints: Seq[ExhaleComponentProofHint], afterExhaleHints: Seq[ExhaleComponentProofHint]) extends AtomicExhaleHint

case object NotSupportedAtomicExhaleHint extends AtomicExhaleHint