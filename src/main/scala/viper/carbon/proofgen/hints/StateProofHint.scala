package viper.carbon.proofgen.hints

import viper.carbon.boogie.{Exp, Var}
import viper.carbon.modules.components.CarbonStateComponentIdentifier

trait StateProofHint

case class UpdateStateComponentHint(stateComponentIdentifier: CarbonStateComponentIdentifier, newStateComponent: Seq[Var], oldStateComponent: Seq[Exp]) extends StateProofHint