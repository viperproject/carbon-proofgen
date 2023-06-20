package viper.carbon.proofgen.hints

case class MethodProofHint(preconditionInhaleHint: Seq[InhaleHint], bodyHint: StmtProofHint)
