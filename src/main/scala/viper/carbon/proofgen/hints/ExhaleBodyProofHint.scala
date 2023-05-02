package viper.carbon.proofgen.hints

import viper.carbon.boogie.LocalVar
import viper.silver.{ast => sil}

trait ExhaleProofHint

case class DefaultExhaleProofHint(bodyHint: Seq[ExhaleBodyProofHint], setupWellDefStateHint: Seq[StateProofHint], exhaleHeapOpt: Option[LocalVar]) extends ExhaleProofHint

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