package viper.carbon.proofgen.hints

import viper.carbon.boogie.{Exp, Var}
import viper.carbon.modules.components.CarbonStateComponentIdentifier

trait StateProofHint

case class UpdateStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier, newStateComponent: Seq[Var], oldStateComponent: Seq[Exp]) extends StateProofHint
case class ResetStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier, stateComponent: Seq[Var]) extends StateProofHint

/**
  * no change state is applied for [[IdentityStateComponentHint]] for the component identified by [[stateComponentIdentifier]]
  */
case class IdentityStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier) extends StateProofHint