package viper.carbon.proofgen.hints

import viper.carbon.boogie.LocalVar
import viper.carbon.{boogie => bpl}
import viper.silver.{ast => sil}

sealed trait StmtProofHint

case class ScopeProofHint(scopedDecls: Seq[sil.Declaration], varsBpl: Seq[LocalVar], bodyHint: StmtProofHint) extends StmtProofHint

case class SeqnProofHint(hints: Seq[StmtProofHint]) extends StmtProofHint

case class IfHint(cond: sil.Exp, componentHints: Seq[StmtComponentProofHint]) extends StmtProofHint

case class WhileHint(cond: sil.Exp, invs: Seq[sil.Exp], componentHints: Seq[StmtComponentProofHint]) extends StmtProofHint

case class AtomicHint(atomicHint: AtomicStmtProofHint) extends StmtProofHint

sealed trait AtomicStmtProofHint

case class LocalVarAssignHint(assignVpr: sil.LocalVarAssign,
                              lhsBpl: bpl.LocalVar, //corresponding Boogie variable
                              hints: Seq[StmtComponentProofHint]
                             ) extends AtomicStmtProofHint
case class FieldAssignHint(fieldAssignVpr: sil.FieldAssign, hints: Seq[StmtComponentProofHint]) extends AtomicStmtProofHint
case class InhaleStmtHint(hints: Seq[StmtComponentProofHint]) extends AtomicStmtProofHint
case class ExhaleStmtHint(hints: Seq[StmtComponentProofHint]) extends AtomicStmtProofHint
case class AssertStmtHint(hints: Seq[StmtComponentProofHint]) extends AtomicStmtProofHint
case class MethodCallHint(hints: Seq[StmtComponentProofHint]) extends AtomicStmtProofHint