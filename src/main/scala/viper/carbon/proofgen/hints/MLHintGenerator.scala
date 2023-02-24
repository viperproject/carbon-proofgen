package viper.carbon.proofgen.hints

import isabelle.ast.MLUtil
import viper.carbon.proofgen.IsaBoogieProcAccessor

object MLHintGenerator {

  def generateHintsInML(stmtProofHint: StmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    stmtProofHint match {
      case LocalVarAssignHint(assignVpr, lhsBpl, hints) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLookupThyThm(lhsBpl)))))
      case SeqnProofHint(hints, scopedDecls) =>
        MLUtil.app("SeqnHint", MLUtil.createList(hints.map(hint => generateHintsInML(hint, boogieProcAccessor, expWfRelInfo, expRelInfo))))
      case IfHint(cond, componentHints) =>
        componentHints match {
          case Seq(IfComponentHint(thnHint, elsHint)) =>
            val thnHintString = generateHintsInML(thnHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
            val elsHintString = generateHintsInML(elsHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
            MLUtil.app("IfHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, thnHintString, elsHintString)))
          case _ => sys.error("if hint has unexpected form")
        }
      case WhileHint(cond, invs, componentHints) => ???
    }
  }
}
