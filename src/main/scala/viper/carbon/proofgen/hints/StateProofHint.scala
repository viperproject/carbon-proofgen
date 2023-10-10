package viper.carbon.proofgen.hints

import viper.carbon.boogie.{Exp, Var}
import viper.carbon.modules.components.CarbonStateComponentIdentifier
import viper.carbon.modules.impls.{HeapStateComponent, PermissionStateComponent}

trait StateProofHint

case class UpdateStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier, newStateComponent: Seq[Var], oldStateComponent: Seq[Exp]) extends StateProofHint
case class ResetStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier, stateComponent: Seq[Var]) extends StateProofHint

/**
  * no change state is applied for [[IdentityStateComponentHint]] for the component identified by [[stateComponentIdentifier]]
  */
case class IdentityStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier) extends StateProofHint

object StateProofHintTactics {
  def updateDefVarTactic(stateComponentIdentifier: CarbonStateComponentIdentifier) : Option[String] = {
    stateComponentIdentifier match {
      case HeapStateComponent => Some("upd_heap_def_var_tac")
      case PermissionStateComponent => Some("upd_mask_def_var_tac")
    }
  }

  def captureDefEvalVarTactic(stateComponentIdentifier: CarbonStateComponentIdentifier): Option[String] = {
    stateComponentIdentifier match {
      case HeapStateComponent => Some("capture_heap_var_tac")
      case PermissionStateComponent => Some("capture_mask_var_tac")
    }
  }

}