package viper.carbon.proofgen.hints

import isabelle.ast.MLUtil
import viper.carbon.proofgen.IsaBoogieProcAccessor

object MLHintGenerator {

  def generateHintsInML(stmtProofHint: StmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expRelInfo: String) : String = {
    stmtProofHint match {
      case LocalVarAssignHint(assignVpr, lhsBpl, hints) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLookupThyThm(lhsBpl)))))
      case SeqnProofHint(hints, scopedDecls) => ???
      case IfHint(cond, componentHints) => ???
      case WhileHint(cond, invs, componentHints) => ???
    }
  }
}
