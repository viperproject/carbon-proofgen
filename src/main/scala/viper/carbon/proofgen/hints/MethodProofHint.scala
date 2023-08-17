package viper.carbon.proofgen.hints

case class MethodProofHint(preconditionInhaleHint: InhaleProofHint,
                           postconditionFramingHint: (Seq[StateProofHint], InhaleProofHint),
                           bodyHint: StmtProofHint,
                           postconditionExhaleHint: ExhaleProofHint)
