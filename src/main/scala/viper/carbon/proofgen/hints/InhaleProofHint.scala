package viper.carbon.proofgen.hints

import viper.silver.{ast => sil}

/** Inhale */
sealed trait InhaleHint
case class StarInhaleHint(left: InhaleHint, right: InhaleHint) extends InhaleHint
case class ImpInhaleHint(cond: sil.Exp, right: InhaleHint) extends InhaleHint
case class CondInhaleHint(cond: sil.Exp, thn: InhaleHint, els: InhaleHint) extends InhaleHint

sealed trait AtomicInhaleHint extends InhaleHint
case class FieldAccessPredicateInhaleHint(fieldAccessPred: sil.FieldAccessPredicate, hints: Seq[InhaleComponentProofHint]) extends AtomicInhaleHint
case class PureExpInhaleHint(e: sil.Exp, hints: Seq[InhaleComponentProofHint]) extends AtomicInhaleHint
case object NotSupportedInhaleHint extends AtomicInhaleHint
