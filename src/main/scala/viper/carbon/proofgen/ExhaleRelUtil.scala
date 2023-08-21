package viper.carbon.proofgen

import isabelle.ast.{MLUtil, ProofUtil}

object ExhaleRelUtil {

  def exhStmtRelThm(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      "exhale_stmt_rel_inst_no_inv"
    } else {
      "exhale_stmt_rel_inst_framing_inv"
    }
  def isExhRelInvThm(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      "true_is_assertion_red_invariant_exh"
    } else {
      ProofUtil.OF("framing_exh_is_assertion_red_invariant_exh", "true_mono_prop_downward")
    }
  def exhNoDefChecksTacOpt(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      MLUtil.none
    } else {
      MLUtil.some("exh_no_def_checks_tac")
    }

  val stmtRelExhaleTrueThm = "exhale_true_stmt_rel"

}
