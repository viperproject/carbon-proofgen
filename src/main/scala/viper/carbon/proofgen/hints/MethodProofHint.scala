package viper.carbon.proofgen.hints

case class MethodProofHint(preconditionInhaleHint: InhaleProofHint,
                           postconditionFramingHint: (Seq[StateProofHint], InhaleProofHint),
                           setupOldStateHint: Seq[StateProofHint],
                           bodyHint: StmtProofHint,
                           postconditionExhaleHint: ExhaleProofHint)
