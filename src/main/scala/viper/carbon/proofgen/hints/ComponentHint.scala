package viper.carbon.proofgen.hints

import viper.silver.{ast => sil}
import viper.carbon.{boogie => bpl}

sealed trait StmtComponentProofHint

case class IfComponentHint(thn: StmtProofHint, els: StmtProofHint) extends StmtComponentProofHint

case class InhaleStmtComponentHint(inhaleHint: InhaleHint) extends StmtComponentProofHint

sealed trait InhaleComponentProofHint