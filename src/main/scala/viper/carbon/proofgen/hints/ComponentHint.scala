package viper.carbon.proofgen.hints

import viper.silver.{ast => sil}
import viper.carbon.{boogie => bpl}

sealed trait ComponentProofHint

case class IfComponentHint(thn: StmtProofHint, els: StmtProofHint) extends ComponentProofHint