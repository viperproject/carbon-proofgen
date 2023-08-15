package viper.carbon.proofgen.hints

import viper.carbon.boogie.LocalVar
import viper.silver.{ast => sil}

sealed trait ExhaleProofHint

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

sealed trait ExhaleBodyProofHint

case class StarExhaleHint(left: ExhaleBodyProofHint, right: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class ImpExhaleHint(cond: sil.Exp, right: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class CondExhaleHint(cond: sil.Exp, thn: ExhaleBodyProofHint, els: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class ForallExhaleHint(f: sil.Forall, bodyHint: ExhaleBodyProofHint) extends ExhaleBodyProofHint
case class UnfoldingExhaleHint(u: sil.Unfolding, beforeUnfoldingHints: Seq[ExhaleComponentProofHint], bodyHint: ExhaleBodyProofHint, afterUnfoldingHints: Seq[ExhaleComponentProofHint]) extends ExhaleBodyProofHint

sealed trait AtomicExhaleHint extends ExhaleBodyProofHint
case class FieldAccessPredicateExhaleHint(fieldAccessPred: sil.FieldAccessPredicate, hintsBefore: Seq[ExhaleComponentProofHint], hintsAfter: Seq[ExhaleComponentProofHint]) extends AtomicExhaleHint
case class PureExpExhaleHint(e: sil.Exp, beforeExhaleHints: Seq[ExhaleComponentProofHint], afterExhaleHints: Seq[ExhaleComponentProofHint]) extends AtomicExhaleHint

case object NotSupportedExhaleHint extends AtomicExhaleHint