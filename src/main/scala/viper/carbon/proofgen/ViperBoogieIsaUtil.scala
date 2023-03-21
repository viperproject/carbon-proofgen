package viper.carbon.proofgen

import isabelle.ast.{IsaTermUtil}
import isabelle.{ast => isa}

object ViperBoogieIsaUtil {

  def allVarsInListBoundedBy(list: isa.Term, maxTerm: BigInt) : isa.Term = {
    IsaTermUtil.listAll(
      isa.TermQuantifier(isa.Lambda, Seq(isa.SimpleIdentifier("x")),
        isa.TermBinary(isa.Le, IsaTermUtil.snd(isa.TermIdent("x")), isa.NatConst(maxTerm))),
      list
    )
  }

  /***
    *
    * @param termList term list that only contains [[isa.TermTuple]] elements where the second element of the tuple is always
    *                 of type [[isa.NatConst]]
    * @return
    */
  def maxInRangeOfList(termList: isa.TermList): BigInt =
    termList.list.map(t => t.asInstanceOf[isa.TermTuple].list(1).asInstanceOf[isa.NatConst].n).max

}
