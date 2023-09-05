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

  val simplifyAstToProgramPointTac : String =
    ProofUtil.simpTacOnly(Seq("convert_ast_to_program_point.simps", "convert_list_to_cont.simps"))

  val unfoldASTBlockInGoalTac : String =
    MLUtil.mlTacticToIsa(MLUtil.app("unfold_bigblock_in_goal", Seq(MLUtil.contextAniquotation, "1")))

  val redAstReflTac : String = ProofUtil.ruleTac("red_ast_bpl_refl")
  val redAstPropagateRelThm : String = "red_ast_bpl_propagate_rel"
  val redAstPropagateRelTac : String = ProofUtil.ruleTac(redAstPropagateRelThm)
  val redAstOneSimpleCmdTac : String = ProofUtil.ruleTac("red_ast_bpl_one_simple_cmd")
  val assignIntroAltTac : String = ProofUtil.ruleTac("assign_intro_alt")

  val redAstBplRelTransitiveTac : String = ProofUtil.ruleTac("red_ast_bpl_rel_transitive")
  val redAstBplRelOneSimpleCmdTac: String = ProofUtil.ruleTac("red_ast_bpl_rel_one_simple_cmd")
  val redAstBplRelIfNondetToElseBranchTac: String = ProofUtil.ruleTac("red_ast_bpl_rel_if_nondet_else")
  def redAstBplRelOutputStatelRelInstantiationTac(stateRelInstantiation: StateRelInstantiation) : String = {
    stateRelInstantiation match {
      case IdentityStateRelInst => ProofUtil.ruleTac("red_ast_bpl_rel_inst_state_rel_same")
      case SecondConjunctStateRelInst => ProofUtil.ruleTac("red_ast_bpl_rel_inst_state_rel_conjunct2")
    }
  }

  val redVarThm : String = "Semantics.RedVar"

  val mapOfLookupVarDeclsTyThm : String = "map_of_lookup_vdecls_ty"

}
