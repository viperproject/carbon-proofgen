package viper.carbon.proofgen

import isabelle.ast.{MLUtil, ProofUtil}

object InhaleRelUtil {

  def inhStmtRelThm(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      "inhale_stmt_rel_no_inv"
    } else {
      "inhale_stmt_rel_inst_framing_inv"
    }

  def isInhRelInvThm(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      "true_is_inh_rel_invariant"
    } else {
      "assertion_framing_is_inh_rel_invariant"
    }
  def inhNoDefChecksTacOpt(includeWellDefChecks: Boolean) : String =
    if(includeWellDefChecks) {
      MLUtil.none
    } else {
      MLUtil.some("inh_no_def_checks_tac")
    }

}
