package viper.carbon.proofgen

import isabelle.ast._

object BoogieIsaTerm {

  def funInterpWf(typeInterp: Term, funDecls: Term, funInterp: Term) : Term =
    TermApp(TermIdent("fun_interp_wf"), Seq(typeInterp, funDecls, funInterp))

  def lookupVarTy(varContext: Term, varName: Term) : Term =
    TermApp(TermIdent("lookup_var_ty"), varContext, varName)

  def wfTy(ty: Term) : Term =
    TermApp(TermIdent("wf_ty"), NatConst(0), ty)


  def astBlock(name: Option[Term], simpleCommands: Term, structuralCommand: Option[Term], transferCommand: Option[Term]): Term = {
    TermApp(
      TermIdent("BigBlock"),
      Seq(IsaTermUtil.convertOpt(name), simpleCommands, IsaTermUtil.convertOpt(structuralCommand), IsaTermUtil.convertOpt(transferCommand))
    )
  }

  val emptyAstBlock = astBlock(None, TermList(Seq()), None, None)

  val emptyContinuation = TermIdent("KStop")

  def programPoint(astBlock: Term, continuation: Term) = TermTuple(Seq(astBlock, continuation))

  def finalProgramPoint = programPoint(emptyAstBlock, emptyContinuation)

}
