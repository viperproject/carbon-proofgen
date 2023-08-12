package viper.carbon.proofgen.hints

import isabelle.ast.{IsaTermUtil, IsaUtil, MLUtil, ProofUtil, StringConst}
import viper.carbon.boogie.LocalVar
import viper.carbon.modules.impls.{HeapStateComponent, PermissionStateComponent}
import viper.carbon.proofgen.{ExhaleRelUtil, IsaBoogieProcAccessor, ViperBoogieIsaUtil, ViperBoogieMLUtil, ViperBoogieRelationIsa}

object MLHintGenerator {

  def generateAtomicInhaleHintsInML(atomicInhaleHint: AtomicInhaleHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    atomicInhaleHint match {
      case FieldAccessPredicateInhaleHint(fieldAccessPred, hints) => {
          val mainComponentHintOpt = hints.find(hint => hint.isInstanceOf[InhaleMainComponentHint])
          mainComponentHintOpt match {
            case Some(mainComponentHint: InhaleMainComponentHint) =>
              val lookupAuxVarTyThm =  boogieProcAccessor.getLocalLookupTyThm(mainComponentHint.temporaryPermVar)
              MLUtil.app("FieldAccInhHint",
                MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(lookupAuxVarTyThm)))
              )
            case _ => sys.error("inhale field access predicate hint missing main component hint")
          }
        }
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

  def generateAtomicExhaleHintsInML(atomicExhaleHint: AtomicExhaleHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    atomicExhaleHint match {
      case FieldAccessPredicateExhaleHint(acc, hintsBefore, _) => {
        val mainComponentHintOpt = hintsBefore.find(hint => hint.isInstanceOf[ExhaleMainComponentHint])
        mainComponentHintOpt match {
          case Some(mainComponentHint: ExhaleMainComponentHint) =>
            val lookupAuxVarTyThm = MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupTyThm(mainComponentHint.temporaryPermVar))
            val auxVarId = boogieProcAccessor.getVarId(mainComponentHint.temporaryPermVar)

            val stateRelAuxVarInstThm = MLUtil.isaToMLThm(ProofUtil.where(ViperBoogieIsaUtil.stateRelAuxVarLookupThm, "aux_var", auxVarId.toString))

            val expRelPermAccessInstThm = MLUtil.isaToMLThm(ProofUtil.where(ViperBoogieIsaUtil.expRelPermAccessThm, "f", StringConst(acc.loc.field.name)))

            MLUtil.app("FieldAccExhHint",
              MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, lookupAuxVarTyThm, stateRelAuxVarInstThm, expRelPermAccessInstThm))
            )
          case _ => sys.error("exhale field access predicate hint missing main component hint")
        }
      }
      case PureExpExhaleHint(e, hintsBefore, hintsAfter) =>
        MLUtil.app("PureExpExhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo)))
    }
  }

  def generateExhaleHintsInML(exhaleHint: ExhaleProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    exhaleHint match {
      //TODO: support case where the body hint contains more than one element and where the heap is not havoced
      case DefaultExhaleProofHint(Seq(bodyHint), includeWellDefChecks, setupWellDefStateHint, Some(exhaleHeapVar)) =>
        val exhaleBodyHint = generateExhaleBodyHintsInML(bodyHint, boogieProcAccessor, expWfRelInfo, expRelInfo)

        val basicInfo = "basic_info"
        val proofCtxt = "ctxt"

        val setupWellDefStateTactics =
          setupWellDefStateHint.map(stateHint => convertStateHintToTactic(stateHint, boogieProcAccessor, basicInfo))

        val setupWellDefTacticFull = MLUtil.lambda(Seq(basicInfo, proofCtxt), ViperBoogieMLUtil.everyRedAstBplRelTransitiveReflTac(proofCtxt, setupWellDefStateTactics))

        createExhaleRelCompleteHint(
          setupWellDefStateTac = setupWellDefTacticFull,
          lookupDeclExhaleHeapThm = MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupDeclThm(exhaleHeapVar)),
          exhaleStmtRelThm = MLUtil.isaToMLThm(ExhaleRelUtil.exhStmtRelThm(includeWellDefChecks)), //TODO: permit optimizations
          exhaleBodyRelHint = exhaleBodyHint
        )
      case _ => sys.error("exhale proof hint has unexpected form")
    }
  }

  def createExhaleRelCompleteHint(setupWellDefStateTac: String, exhaleStmtRelThm: String, lookupDeclExhaleHeapThm: String, exhaleBodyRelHint: String) : String = {
    MLUtil.createRecord(
      Seq(
        ("setup_well_def_state_tac", setupWellDefStateTac),
        ("exhale_stmt_rel_thm", exhaleStmtRelThm),
        ("lookup_decl_exhale_heap", lookupDeclExhaleHeapThm),
        ("exhale_rel_hint", exhaleBodyRelHint)
      )
    )
  }

  def generateExhaleBodyHintsInML(exhaleBodyHint: ExhaleBodyProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    exhaleBodyHint match {
      case StarExhaleHint(left, right) =>
        val leftHintString = generateExhaleBodyHintsInML(left, boogieProcAccessor, expWfRelInfo, expRelInfo)
        val rightHintString = generateExhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("StarExhHint", MLUtil.createTuple(Seq(leftHintString, rightHintString)))
      case ImpExhaleHint(cond, right) =>
        val rightHintString = generateExhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("ImpExhHint", Seq(expWfRelInfo, expRelInfo, rightHintString))
      case CondExhaleHint(cond, thn, els) => ???
      case a: AtomicExhaleHint => MLUtil.app("AtomicExhHint", generateAtomicExhaleHintsInML(a, boogieProcAccessor, expWfRelInfo, expRelInfo))
    }
  }

  // converts a state hint to a tactic that takes a proof context as input
  def convertStateHintToTactic(stateHint: StateProofHint, boogieProcAccessor: IsaBoogieProcAccessor, basicInfo: String) : String = {
    stateHint match {
      case UpdateStateComponentHint(stateComponentIdentifier, Seq(newVar), _) =>
        val newVarLookupThm = MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupTyThm(newVar.asInstanceOf[LocalVar]))

        stateComponentIdentifier match {
          case HeapStateComponent =>  MLUtil.app("upd_heap_def_var_tac", Seq(newVarLookupThm, basicInfo))
          case PermissionStateComponent => MLUtil.app("upd_mask_def_var_tac", Seq(newVarLookupThm, basicInfo))
          case _ => sys.error("unsupported state component identifier")
        }
      case _ => sys.error("state hint has unexpected form")
    }
  }

  def generateAtomicStmtHintsInML(atomicProofHint: AtomicStmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String ={
    atomicProofHint match {
      case LocalVarAssignHint(assignVpr, lhsBpl, hints) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupTyThm(lhsBpl)))))
      case FieldAssignHint(fieldAssignVpr, hints) =>
        MLUtil.app("FieldAssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expWfRelInfo, expRelInfo, expRelInfo)))
      case InhaleStmtHint(componentHints) =>
        componentHints match {
          case Seq(InhaleStmtComponentHint(inhaleHint)) =>
            MLUtil.app("InhaleHint", generateInhaleHintsInML(inhaleHint, boogieProcAccessor, expWfRelInfo, expRelInfo))
          case _ => sys.error("inhale hint has unexpected form")
        }
      case ExhaleStmtHint(componentHints) =>
        componentHints match {
          case Seq(ExhaleStmtComponentHint(exhaleHint)) =>
            MLUtil.app("ExhaleHint", generateExhaleHintsInML(exhaleHint, boogieProcAccessor, expWfRelInfo, expRelInfo))
          case _ => sys.error("exhale hint has unexpected form")
        }
      case MethodCallHint(componentHints) =>
        "(MethodCallHint)" //TODO

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
