package viper.carbon.proofgen

import isabelle.ast.MLUtil

object ViperBoogieMLUtil {

  def genTypeSafetyThmMap(funInterpWf: String, funDeclsWf: String, varContextWf: String, stateWf: String) : String = {
    MLUtil.app("gen_type_safety_thm_map", Seq(funInterpWf, funDeclsWf, varContextWf, stateWf))
  }

  def createExpRelInfo(typeSafetyThmMap: String, lookupVarRelTac: String, vprLitBplExpRelTac: String, lookupVarThms: String) : String =
    MLUtil.app("ExpRelInfo0", MLUtil.createTuple(Seq(typeSafetyThmMap, lookupVarRelTac, vprLitBplExpRelTac, lookupVarThms)))


  def createStmtRelInfo(ctxtWfThm: String, trDefThm: String, varRelTac: String, varContextVprTac: String) : String =
      MLUtil.createRecord(Seq(
        ("ctxt_wf_thm", ctxtWfThm),
        ("tr_def_thm", trDefThm),
        ("var_rel_tac", varRelTac),
        ("var_context_vpr_tac", varContextVprTac)
      ))

}
