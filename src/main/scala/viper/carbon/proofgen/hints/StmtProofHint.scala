package viper.carbon.proofgen.hints

import viper.carbon.{boogie => bpl}
import viper.silver.{ast => sil}

sealed trait StmtProofHint { }

case class LocalVarAssignHint(assignVpr: sil.LocalVarAssign,
                              lhsBpl: bpl.LocalVar, //corresponding Boogie variable
                              hints: Seq[ComponentProofHint]
                     ) extends StmtProofHint

case class SeqnProofHint(hints: Seq[StmtProofHint], scopedDecls: Seq[sil.Declaration]) extends StmtProofHint

case class IfHint(cond: sil.Exp, componentHints: Seq[ComponentProofHint]) extends StmtProofHint

case class WhileHint(cond: sil.Exp, invs: Seq[sil.Exp], componentHints: Seq[ComponentProofHint]) extends StmtProofHint

//the following hint should only occur for Viper methods that contain statements not supported by proof generation
case class ProofNotSupportedHint(unsupportedNode: sil.Stmt) extends StmtProofHint