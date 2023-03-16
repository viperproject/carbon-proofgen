package viper.carbon.proofgen.hints

import isabelle.ast.MLUtil
import viper.carbon.proofgen.IsaBoogieProcAccessor

object MLHintGenerator {

  def generateAtomicInhaleHintsInML(atomicInhaleHint: AtomicInhaleHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    atomicInhaleHint match {
      case FieldAccessPredicateInhaleHint(fieldAccessPred, hints) =>
        MLUtil.app("FieldAccInhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo)))
      case PureExpInhaleHint(e, hints) =>
        MLUtil.app("PureExpInhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo)))
    }
  }

  def generateInhaleHintsInML(inhaleHint: InhaleHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    inhaleHint match {
      case StarInhaleHint(left, right) =>
        val leftHintString = generateInhaleHintsInML(left, boogieProcAccessor, expWfRelInfo, expRelInfo)
        val rightHintString = generateInhaleHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("StarInhHint", MLUtil.createTuple(Seq(leftHintString, rightHintString)))
      case ImpInhaleHint(cond, right) =>
        val rightHintString = generateInhaleHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("ImpInhHint", Seq(expWfRelInfo, expRelInfo, rightHintString))
      case CondInhaleHint(cond, thn, els) => ???
      case a: AtomicInhaleHint => MLUtil.app("AtomicInhHint", generateAtomicInhaleHintsInML(a, boogieProcAccessor, expWfRelInfo, expRelInfo))
    }
  }

  def generateAtomicStmtHintsInML(atomicProofHint: AtomicStmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String ={
    atomicProofHint match {
      case LocalVarAssignHint(assignVpr, lhsBpl, hints) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLookupThyThm(lhsBpl)))))
      case FieldAssignHint(fieldAssignVpr, hints) =>
        MLUtil.app("FieldAssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expWfRelInfo, expRelInfo, expRelInfo)))
      case InhaleStmtHint(componentHints) =>
        componentHints match {
          case Seq(InhaleStmtComponentHint(inhaleHint)) =>
            MLUtil.app("InhaleHint", generateInhaleHintsInML(inhaleHint, boogieProcAccessor, expWfRelInfo, expRelInfo))
          case _ => sys.error("inhale hint has unexpected form")
        }
    }
  }

  def generateStmtHintsInML(stmtProofHint: StmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    stmtProofHint match {
      case SeqnProofHint(hints, scopedDecls) =>
        MLUtil.app("SeqnHint", MLUtil.createList(hints.map(hint => generateStmtHintsInML(hint, boogieProcAccessor, expWfRelInfo, expRelInfo))))
      case IfHint(cond, componentHints) =>
        componentHints match {
          case Seq(IfComponentHint(thnHint, elsHint)) =>
            val thnHintString = generateStmtHintsInML(thnHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
            val elsHintString = generateStmtHintsInML(elsHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
            MLUtil.app("IfHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, thnHintString, elsHintString)))
          case _ => sys.error("if hint has unexpected form")
        }
      case WhileHint(cond, invs, componentHints) => ???
      case AtomicHint(atomicHint) =>
        MLUtil.app("AtomicHint" ,generateAtomicStmtHintsInML(atomicHint, boogieProcAccessor, expWfRelInfo, expRelInfo))
    }
  }

}
