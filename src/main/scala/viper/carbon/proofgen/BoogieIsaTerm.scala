package viper.carbon.proofgen

import isabelle.ast._

object BoogieIsaTerm {

  val funInterpWfId: TermIdent = TermIdent("fun_interp_wf")
  val funInterpSingleWfId: TermIdent = TermIdent("fun_interp_single_wf")

  val funInterpSingleWf2Id: TermIdent = TermIdent("fun_interp_single_wf_2")

  def funInterpWf(typeInterp: Term, funDecls: Term, funInterp: Term) : Term =
    TermApp(funInterpWfId, Seq(typeInterp, funDecls, funInterp))

  def lookupVarTy(varContext: Term, varName: Term) : Term =
    TermApp(TermIdent("lookup_var_ty"), varContext, varName)

  def lookupVarDeclsTy(varContext: Term, varName: Term) : Term =
    TermApp(TermIdent("lookup_vdecls_ty"), varContext, varName)

  def wfTy(ty: Term) : Term =
    TermApp(TermIdent("wf_ty"), NatConst(0), ty)

  def lookupVar(varContext: Term, normalState: Term, varName: Term) : Term =
    TermApp(TermIdent("lookup_var"), Seq(varContext, normalState, varName))

  val abstractField : TermIdent = TermIdent("AField")
  val normalField : TermIdent = TermIdent("NormalField")

  def normalFieldAbstractValue(constId: Term, vprType: Term) : Term =
    abstractValueConstr(TermApp(abstractField, TermApp(normalField, constId, vprType)))

  def abstractValueConstr(absValue: Term) = TermApp(TermIdent("AbsV"), absValue)

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

  val closedWfTyFunEqThm : String = "closed_wf_ty_fun_eq"

  def typeInterpBplAbbrev(name: String) : AbbrevDecl =
    AbbrevDecl(
      name,
      None,
      (Seq(TermIdent("A")), ViperBoogieRelationIsa.viperBoogieAbstractTypeInterp(TypeRepresentation.makeBasicTypeRepresentation(TermIdent("A"))))
    )

  val axiomsSatId : TermIdent = TermIdent("axioms_sat")

  def axiomsSat(typeInterp: Term, varContext: Term, funInterp: Term, normalState: Term, axioms: Term)  =
    TermApp(axiomsSatId, Seq(typeInterp, varContext, funInterp, normalState, axioms))

  def procIsCorrect( typeInterp: Term,
                     functionDecls: Term,
                     constDecls: Term,
                     uniqueConstNames: Term,
                     globalVarDecls: Term,
                     axiomDecls: Term,
                     proc: Term,
                     vprDomainValueType: TypeIsa
                   ): Term =
  {
    TermApp(TermIdent("proc_is_correct"),
      Seq(typeInterp, functionDecls, constDecls, uniqueConstNames, globalVarDecls, axiomDecls, proc,
        TermWithExplicitType(
          TermIdent("Ast.proc_body_satisfies_spec"),
          DataType("proc_body_satisfies_spec_ty", Seq(ViperBoogieRelationIsa.viperBoogieAbstractValueType(vprDomainValueType), BoogieIsaType.astType))
        )
    ))
  }

}
