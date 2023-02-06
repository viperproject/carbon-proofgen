package viper.carbon.proofgen.hints

import isabelle.ast.MLUtil
import viper.carbon.proofgen.IsaBoogieProcAccessor

object MLHintGenerator {

  def generateHintsInML(stmtProofHint: StmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expRelInfo: String) : String = {
    stmtProofHint match {
      case LocalVarAssignHint(assignVpr, lhsBpl, hints) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLookupThyThm(lhsBpl)))))
      case SeqnProofHint(hints, scopedDecls) =>
        MLUtil.app("SeqnHint", MLUtil.createList(hints.map(hint => generateHintsInML(hint, boogieProcAccessor, expRelInfo))))
      case IfHint(cond, componentHints) =>
        componentHints match {
          case Seq(IfComponentHint(thnHint, elsHint)) =>
            val thnHintString = generateHintsInML(thnHint, boogieProcAccessor, expRelInfo)
            val elsHintString = generateHintsInML(elsHint, boogieProcAccessor, expRelInfo)
            MLUtil.app("IfHint", MLUtil.createTuple(Seq(expRelInfo, thnHintString, elsHintString)))
          case _ => sys.error("if hint has unexpected form")
        }
      case WhileHint(cond, invs, componentHints) => ???
    }
  }
}
