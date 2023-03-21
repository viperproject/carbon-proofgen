package viper.carbon.proofgen

import isabelle.ast.MLUtil

object ViperBoogieMLUtil {

  def genTypeSafetyThmMap(funInterpWf: String, funDeclsWf: String, varContextWf: String, stateWf: String) : String = {
    MLUtil.app("gen_type_safety_thm_map", Seq(funInterpWf, funDeclsWf, varContextWf, stateWf))
  }

  def createExpRelInfo(typeSafetyThmMap: String, lookupVarRelTac: String, vprLitBplExpRelTac: String, lookupVarThms: String, lookupFunBplThms: String, fieldAccessRelPreTac: String) : String =
    MLUtil.createRecord(
      Seq(
        ("type_safety_thm_map", typeSafetyThmMap),
        ("lookup_var_rel_tac", lookupVarRelTac),
        ("vpr_lit_bpl_exp_rel_tac", vprLitBplExpRelTac),
        ("lookup_var_thms", lookupVarThms),
        ("lookup_fun_bpl_thms", lookupFunBplThms),
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
                             varRelTac: String,
                             varContextVprTac: String,
                             fieldRelSingleTac: String,
                             auxVarDisjTac: String,
                             tyInterpEContextBplEq: String) : String =
      MLUtil.createRecord(Seq(
        ("ctxt_wf_thm", ctxtWfThm),
        ("tr_def_thm", trDefThm),
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

  def createStmtRelInfo(basicStmtRelInfo: String, atomicRelTac: String, inhaleRelInfo: String) : String =
    MLUtil.createRecord(Seq(
      ("basic_stmt_rel_info", basicStmtRelInfo),
      ("atomic_rel_tac", atomicRelTac),
      ("inhale_rel_info", inhaleRelInfo)
    ))
}
