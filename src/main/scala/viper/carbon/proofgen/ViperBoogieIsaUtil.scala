package viper.carbon.proofgen

import isabelle.ast.{IsaTermUtil}
import isabelle.{ast => isa}

object ViperBoogieIsaUtil {

  val stateRelAuxVarLookupThm = "state_rel_aux_pred_sat_lookup_3"

  val expRelPermAccessThm = "exp_rel_perm_access_2"

  val constReprBoundLemmaName = "const_repr_basic_bound_2"

  def allVarsInListBoundedBy(list: isa.Term, minTerm: BigInt, maxTerm: BigInt) : isa.Term = {
    IsaTermUtil.listAll(
      isa.TermQuantifier(isa.Lambda, Seq(isa.SimpleIdentifier("x")),
        isa.TermBinary.and(
          isa.TermBinary(isa.Le, isa.NatConst(minTerm), IsaTermUtil.snd(isa.TermIdent("x"))),
          isa.TermBinary(isa.Le, IsaTermUtil.snd(isa.TermIdent("x")), isa.NatConst(maxTerm)),
        )
      ),
      list
    )
  }

  /***
    *
    * @param termList term list that only contains [[isa.TermTuple]] elements where the second element of the tuple is always
    *                 of type [[isa.NatConst]] (i.e., represents a natural number)
    * @return minimum natural number in the list (i.e., of all second elements in the tuples) if the list is non-empty and otherwise 0
    */
  def minInRangeOfList(termList: isa.TermList): BigInt = {
    if(termList.list.isEmpty) {
      0
    } else {
      rangeOfList(termList).min
    }
  }

  /***
    *
    * @param termList term list that only contains [[isa.TermTuple]] elements where the second element of the tuple is always
    *                 of type [[isa.NatConst]] (i.e., represents a natural number)
    * @return maximum natural number in the list (i.e., of all second elements in the tuples) if the list is non-empty and otherwise 0
    */
  def maxInRangeOfList(termList: isa.TermList): BigInt = {
    if(termList.list.isEmpty) {
      0
    } else {
      rangeOfList(termList).max
    }
  }

  /***
    *
    * @param termList term list that only contains [[isa.TermTuple]] elements where the second element of the tuple is always
    *                 of type [[isa.NatConst]] (i.e., represents a natural number)
    * @return all the second elements
    */
  def rangeOfList(termList: isa.TermList): Seq[BigInt] = {
    termList.list.map(t => t.asInstanceOf[isa.TermTuple].list(1).asInstanceOf[isa.NatConst].n)
  }

}
