package viper.carbon.proofgen

import viper.silver.{ast => sil}
import isabelle.{ast => isa}
import viper.carbon.proofgen.hints.{ExhaleBodyProofHint, ExhaleStmtHint, InhaleBodyProofHint, NotSupportedAtomicInhaleHint, StarExhaleHint, StarInhaleHint, TrivialExhaleHint, TrivialInhaleHint}

object IsaMethodSpecificationHelper {

  /**
    * Translates a pre- or postcondition into an Isabelle term, where the pre- or postcondition is expressed by
    * a sequence of assertions representing their separating conjunction.
    * @param spec pre- or postcondition
    * @param varTranslation
    * @return
    */
  def conjoinSpecAssertions(spec: Seq[sil.Exp]) : sil.Exp = {
    conjoinSpec(sil.TrueLit()(), { case (a1, a2) => sil.And(a1, a2)() : sil.Exp } : (sil.Exp, sil.Exp) => sil.Exp, spec)
  }

  def conjoinSpecInhaleHints(specHints: Seq[InhaleBodyProofHint]) : InhaleBodyProofHint = {
    conjoinSpec(TrivialInhaleHint, { case (a1, a2) => StarInhaleHint(a1, a2) } : (InhaleBodyProofHint, InhaleBodyProofHint) => InhaleBodyProofHint, specHints)
  }

  def conjoinSpecExhaleHints(specHints: Seq[ExhaleBodyProofHint]) : ExhaleBodyProofHint = {
    conjoinSpec(TrivialExhaleHint, { case (a1, a2) => StarExhaleHint(a1, a2) } : (ExhaleBodyProofHint, ExhaleBodyProofHint) => ExhaleBodyProofHint, specHints)
  }

  /**
    * Conjoin a sequence of elements representing a information about a specification (pre- or postcondition), where
    * the actual specification is given by the separating conjunction of the assertions associated with the elements.
    * @param empty element to be returned if the input sequence is empty
    * @param conjoin binary function to conjoin two elements
    * @param specElems
    * @tparam A
    * @return
    */
  def conjoinSpec[A](empty: A, conjoin: (A,A) => A, specElems: Seq[A]) : A = {
    if(specElems.isEmpty) {
      empty
    } else {
      val hd = specElems.head
      specElems.tail.foldLeft(hd) { (res, elem) => conjoin(res, elem) }
    }
  }

}
