package viper.carbon.proofgen

import isabelle.ast.MLUtil

object ViperBoogieMLUtil {

  def genTypeSafetyThmMap(funInterpWf: String, funDeclsWf: String, varContextWf: String, stateWf: String) : String = {
    MLUtil.app("gen_type_safety_thm_map", Seq(funInterpWf, funDeclsWf, varContextWf, stateWf))
  }

  def createExpRelInfo(typeSafetyThmMap: String, lookupVarRelTac: String, vprLitBplExpRelTac: String, lookupVarThms: String, lookupFunBplThms: String, simplifyRtypeInterpTac: String, fieldAccessRelPreTac: String) : String =
    MLUtil.createRecord(
      Seq(
        ("type_safety_thm_map", typeSafetyThmMap),
        ("lookup_var_rel_tac", lookupVarRelTac),
        ("vpr_lit_bpl_exp_rel_tac", vprLitBplExpRelTac),
        ("lookup_var_thms", lookupVarThms),
        ("lookup_fun_bpl_thms", lookupFunBplThms),
        ("simplify_rtype_interp_tac", simplifyRtypeInterpTac),
        ("field_access_rel_pre_tac", fieldAccessRelPreTac)
      )
    )

  def createExpWfRelInfo(fieldAccessWfRelSynTac: String) : String =
    MLUtil.createRecord(Seq(("field_access_wf_rel_syn_tac", fieldAccessWfRelSynTac)))

  def fieldAccessRelPreTac(heapReadWfTac: String, heapReadMatchTac: String, fieldRelSingleTac: String) : String =
    MLUtil.app("field_access_rel_pre_tac_aux", Seq(heapReadWfTac, heapReadMatchTac, fieldRelSingleTac))

  def fieldAccessWfRelTacAuxInst(fieldAccInitTac: String, lookupMaskVarTac: String, fieldRelSingleTac: String,
                                 tyArgsEqTac: String, expRelInfo: String) =
    MLUtil.app("field_access_wf_rel_tac_aux", Seq(fieldAccInitTac, lookupMaskVarTac, fieldRelSingleTac, tyArgsEqTac, expRelInfo))

  def createBasicStmtRelInfo(ctxtWfThm: String,
                             trDefThm: String,
                             vprProgramContextEqThm: String,
                             varRelTac: String,
                             varContextVprTac: String,
                             fieldRelSingleTac: String,
                             auxVarDisjTac: String,
                             tyInterpEContextBplEq: String) : String =
      MLUtil.createRecord(Seq(
        ("ctxt_wf_thm", ctxtWfThm),
        ("consistency_wf_thm",  MLUtil.isaToMLThm(ViperBoogieRelationIsa.trivialConsistencyWfThm)),
        ("tr_def_thm", trDefThm),
        ("vpr_program_ctxt_eq_thm", vprProgramContextEqThm),
        ("var_rel_tac", varRelTac),
        ("var_context_vpr_tac", varContextVprTac),
        ("field_rel_single_tac", fieldRelSingleTac),
        ("aux_var_disj_tac", auxVarDisjTac),
        ("type_interp_econtext", tyInterpEContextBplEq)
      ))

  def createInhaleRelInfo(basicStmtRelInfo: String, atomicInhaleRelTac: String) : String =
    MLUtil.createRecord(Seq(
        ("basic_info", basicStmtRelInfo),
        ("atomic_inhale_rel_tac", atomicInhaleRelTac)
      )
    )

  def createExhaleRelInfo(basicStmtRelInfo: String, atomicExhaleRelTac: String, isExhRelInvThm: String) : String =
    MLUtil.createRecord(Seq(
        ("basic_info", basicStmtRelInfo),
        ("atomic_exhale_rel_tac", atomicExhaleRelTac),
        ("is_exh_rel_inv_thm", isExhRelInvThm)
      )
    )

  def createStmtRelInfo(basicStmtRelInfo: String, atomicRelTac: String, inhaleRelInfo: String, exhaleRelInfo: String) : String =
    MLUtil.createRecord(Seq(
      ("basic_stmt_rel_info", basicStmtRelInfo),
      ("atomic_rel_tac", atomicRelTac),
      ("inhale_rel_info", inhaleRelInfo),
      ("exhale_rel_info", exhaleRelInfo)
    ))

  def everyRedAstBplRelTransitiveReflTac(proofContext: String, tactics: Seq[String]) : String =
    s"EVERY'_red_ast_bpl_rel_transitive_refl $proofContext ${MLUtil.createList(tactics)}"

}
