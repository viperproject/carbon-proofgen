package viper.carbon.proofgen.hints

import viper.carbon.boogie.{LocalVar, Var}

sealed trait StmtComponentProofHint

case class IfComponentHint(thn: StmtProofHint, els: StmtProofHint) extends StmtComponentProofHint

case class InhaleStmtComponentHint(inhaleHint: InhaleProofHint) extends StmtComponentProofHint
case class ExhaleStmtComponentHint(exhaleHint: ExhaleProofHint) extends StmtComponentProofHint
case class AssertStmtComponentHint(exhaleHint: ExhaleProofHint) extends StmtComponentProofHint
case class MethodCallStmtComponentHint(
                calleeName: String,
                targetVarsBpl: Seq[Var],
                exhalePreHint: ExhaleProofHint,
                inhalePostHint: InhaleProofHint) extends StmtComponentProofHint

sealed trait InhaleComponentProofHint

case class InhaleMainComponentHint(temporaryPermVar: LocalVar) extends InhaleComponentProofHint

sealed trait ExhaleComponentProofHint
case class ExhaleMainComponentHint(temporaryPermVar: LocalVar) extends ExhaleComponentProofHint
