package viper.carbon.proofgen.hints

case class MethodProofHint(preconditionInhaleHint: InhaleProofHint, bodyHint: StmtProofHint, postconditionExhaleHint: ExhaleProofHint)
