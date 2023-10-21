package viper.carbon.proofgen.hints

import isabelle.ast.{BoolConst, IsaTermUtil, IsaUtil, Lambda, MLUtil, ProofUtil, StringConst, TermList, TermQuantifier, Wildcard}
import viper.carbon.boogie.LocalVar
import viper.carbon.modules.components.{CarbonStateComponent, CarbonStateComponentIdentifier}
import viper.carbon.modules.impls.{HeapStateComponent, PermissionStateComponent}
import viper.carbon.proofgen.{ExhaleRelUtil, InhaleRelUtil, IsaBoogieProcAccessor, ProofGenMLConstants, ViperBoogieIsaUtil, ViperBoogieMLUtil, ViperBoogieRelationIsa}

object MLHintGenerator {

  def generateAtomicInhaleHintsInML(atomicInhaleHint: AtomicInhaleHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    atomicInhaleHint match {
      case FieldAccessPredicateInhaleHint(fieldAccessPred, hints) => {
          val mainComponentHintOpt = hints.find(hint => hint.isInstanceOf[InhaleMainComponentHint])
          mainComponentHintOpt match {
            case Some(mainComponentHint: InhaleMainComponentHint) =>
              val lookupAuxVarTyThm =  boogieProcAccessor.getLocalLookupTyThm(mainComponentHint.temporaryPermVar)

              val auxVarId = boogieProcAccessor.getVarId(mainComponentHint.temporaryPermVar)
              val stateRelAuxVarInstThm = MLUtil.isaToMLThm(ProofUtil.where(ViperBoogieIsaUtil.stateRelAuxVarLookupThm, "aux_var", auxVarId.toString))

              MLUtil.app("FieldAccInhHint",
                MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(lookupAuxVarTyThm), stateRelAuxVarInstThm))
              )
            case _ => sys.error("inhale field access predicate hint missing main component hint")
          }
        }
      case PureExpInhaleHint(e, hints) =>
        MLUtil.app("PureExpInhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo)))
    }
  }

  def generateInhaleBodyHintsInML(inhaleHint: InhaleBodyProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    inhaleHint match {
      case StarInhaleHint(left, right) =>
        val leftHintString = generateInhaleBodyHintsInML(left, boogieProcAccessor, expWfRelInfo, expRelInfo)
        val rightHintString = generateInhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("StarInhHint", MLUtil.createTuple(Seq(leftHintString, rightHintString)))
      case ImpInhaleHint(cond, right) =>
        val rightHintString = generateInhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("ImpInhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, rightHintString)))
      case CondInhaleHint(cond, thn, els) => ???
      case a: AtomicInhaleHint => MLUtil.app("AtomicInhHint", generateAtomicInhaleHintsInML(a, boogieProcAccessor, expWfRelInfo, expRelInfo))
    }
  }

  def generateInhaleHintsInML(inhaleHint: InhaleProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    inhaleHint match {
      case InhaleProofHint(Seq(bodyHint), addWelldefinednessChecks) =>
        val inhaleBodyHint = generateInhaleBodyHintsInML(bodyHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
        createInhaleRelCompleteHint(MLUtil.isaToMLThm(InhaleRelUtil.inhStmtRelThm(addWelldefinednessChecks)), inhaleBodyHint)
    }
  }

  def createInhaleRelCompleteHint(inhaleStmtRelThm: String, inhaleBodyRelHint: String) : String = {
    MLUtil.createRecord(
      Seq(
        ("inhale_stmt_rel_thm", inhaleStmtRelThm),
        ("inhale_rel_hint", inhaleBodyRelHint)
      )
    )
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

  def generateExhaleBodyHintsInML(exhaleBodyHint: ExhaleBodyProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    exhaleBodyHint match {
      case StarExhaleHint(left, right) =>
        val leftHintString = generateExhaleBodyHintsInML(left, boogieProcAccessor, expWfRelInfo, expRelInfo)
        val rightHintString = generateExhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("StarExhHint", MLUtil.createTuple(Seq(leftHintString, rightHintString)))
      case ImpExhaleHint(cond, right) =>
        val rightHintString = generateExhaleBodyHintsInML(right, boogieProcAccessor, expWfRelInfo, expRelInfo)
        MLUtil.app("ImpExhHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, rightHintString)))
      case CondExhaleHint(cond, thn, els) => ???
      case a: AtomicExhaleHint => MLUtil.app("AtomicExhHint", generateAtomicExhaleHintsInML(a, boogieProcAccessor, expWfRelInfo, expRelInfo))
    }
  }

  def generateExhaleHintsInML(exhaleHint: ExhaleProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : ExhaleRelCompleteHint = {
    exhaleHint match {
      //TODO: support case where the body hint contains more than one element
      case DefaultExhaleProofHint(Seq((bodyHint, includeWellDefChecks)), setupWellDefStateHint, exhaleHeapVarOpt) =>
        val exhaleBodyHint = generateExhaleBodyHintsInML(bodyHint, boogieProcAccessor, expWfRelInfo, expRelInfo)

        val basicInfo = "basic_info"
        val proofCtxt = "ctxt"

        val setupWellDefStateTactics =
          setupWellDefStateHint.map(stateHint => convertUpdStateHintToTactic(stateHint, StateProofHintTactics.updateDefVarTactic, boogieProcAccessor, Seq(basicInfo)))

        val setupWellDefTacticFull = MLUtil.lambda(Seq(basicInfo, proofCtxt), ViperBoogieMLUtil.everyRedAstBplRelTransitiveReflTac(proofCtxt, setupWellDefStateTactics))

        createExhaleRelCompleteHint(
          setupWellDefStateTac = setupWellDefTacticFull,
          lookupDeclExhaleHeapThm = exhaleHeapVarOpt.fold(MLUtil.none)(exhaleHeapVar => MLUtil.some(MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupDeclThm(exhaleHeapVar)))),
          exhaleStmtRelThm = MLUtil.isaToMLThm(ExhaleRelUtil.exhStmtRelThm(includeWellDefChecks)), //TODO: permit optimizations
          exhaleBodyRelHint = exhaleBodyHint
        )
      case _ =>
        sys.error("exhale proof hint has unexpected form")
    }
  }

  def generateAssertHintsInML(assertHint: AssertStmtComponentHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo: String, expRelInfo: String): String = {
    assertHint match {
      case AssertStmtComponentHint(setupAssertStateOpt, exhaleHint) => {
        val exhaleCompleteHint = generateExhaleHintsInML(exhaleHint, boogieProcAccessor, expWfRelInfo, expRelInfo)

        setupAssertStateOpt match {
          case Some(setupAssertState) => {
            val setupWellDefStateTactics =
              setupAssertState.map(stateHint =>
                convertUpdStateHintToTactic(stateHint, StateProofHintTactics.captureDefEvalVarTactic, boogieProcAccessor, Seq())
              )

            createAssertRelCompleteHint(
              setupWellDefStateTac = exhaleCompleteHint.setupWellDefStateTac,
              assertStmtRelThm = MLUtil.isaToMLThm(ProofUtil.where("assert_stmt_rel_inst","Q", TermQuantifier(Lambda, Seq(Wildcard, Wildcard, Wildcard), BoolConst(true)).toString)),
              initTac = MLUtil.app("assert_rel_init_tac_standard", MLUtil.createList(setupWellDefStateTactics)),
              exhaleBodyRelHint = exhaleCompleteHint.exhaleBodyRelHint,
              resetStateTac = "assert_rel_reset_state_tac_standard"
            )
          }
          case None =>
            createAssertRelCompleteHint(
              setupWellDefStateTac = exhaleCompleteHint.setupWellDefStateTac,
              assertStmtRelThm = MLUtil.isaToMLThm(ProofUtil.where("assert_stmt_rel_inst","Q", TermQuantifier(Lambda, Seq(Wildcard, Wildcard, Wildcard), BoolConst(true)).toString)),
              initTac = "assert_rel_init_tac_pure",
              exhaleBodyRelHint = exhaleCompleteHint.exhaleBodyRelHint,
              resetStateTac = "assert_rel_reset_state_tac_pure"
            )
        }
      }
      case _ =>
        sys.error("exhale proof hint has unexpected form")
    }
  }

  case class ExhaleRelCompleteHint(setupWellDefStateTac: String, exhaleStmtRelThm: String, lookupDeclExhaleHeapThm: String, exhaleBodyRelHint: String) {
    def getRepresentation : String =  {
      MLUtil.createRecord(
        Seq(
          ("setup_well_def_state_tac", setupWellDefStateTac),
          ("exhale_stmt_rel_thm", exhaleStmtRelThm),
          ("lookup_decl_exhale_heap", lookupDeclExhaleHeapThm),
          ("exhale_rel_hint", exhaleBodyRelHint)
        )
      )
    }
  }

  def createExhaleRelCompleteHint(setupWellDefStateTac: String, exhaleStmtRelThm: String, lookupDeclExhaleHeapThm: String, exhaleBodyRelHint: String): ExhaleRelCompleteHint = {
    ExhaleRelCompleteHint(setupWellDefStateTac: String, exhaleStmtRelThm: String, lookupDeclExhaleHeapThm: String, exhaleBodyRelHint: String)
  }

  def createAssertRelCompleteHint(setupWellDefStateTac: String, assertStmtRelThm: String, initTac: String, exhaleBodyRelHint: String, resetStateTac: String): String = {
    MLUtil.createRecord(
      Seq(
        ("setup_well_def_state_tac", setupWellDefStateTac),
        ("assert_stmt_rel_thm", assertStmtRelThm),
        ("init_tac", initTac),
        ("exhale_rel_hint", exhaleBodyRelHint),
        ("reset_state_tac", resetStateTac)
      )
    )
  }

  // converts a state hint to a tactic that takes a proof context as input
  def convertUpdStateHintToTactic(stateHint: StateProofHint, stateCompToTactic: CarbonStateComponentIdentifier => Option[String],
                                  boogieProcAccessor: IsaBoogieProcAccessor, trailingArgs: Seq[String]) : String = {
    stateHint match {
      case UpdateStateComponentHint(stateComponentIdentifier, Seq(newVar), _) =>
        val newVarLookupThm = MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupTyThm(newVar.asInstanceOf[LocalVar]))

        val tacticOpt  = stateCompToTactic(stateComponentIdentifier)
        tacticOpt match {
          case Some(tactic) => MLUtil.app(tactic, Seq(newVarLookupThm)++trailingArgs)
          case None => sys.error(s"unsupported state component identifier: $stateComponentIdentifier")
        }
      case _ => sys.error("state hint has unexpected form")
    }
  }

  def generateAtomicStmtHintsInML(atomicProofHint: AtomicStmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String ={
    atomicProofHint match {
      case LocalVarAssignHint(_, lhsBpl, _) =>
        MLUtil.app("AssignHint", MLUtil.createTuple(Seq(expWfRelInfo, expRelInfo, MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupTyThm(lhsBpl)))))
      case FieldAssignHint(_, hints) =>
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
            MLUtil.app("ExhaleHint", generateExhaleHintsInML(exhaleHint, boogieProcAccessor, expWfRelInfo, expRelInfo).getRepresentation)
          case _ => sys.error("exhale hint has unexpected form")
        }
      case AssertStmtHint(componentHints) =>
        componentHints match { //TODO: fix, 
          case Seq(a@AssertStmtComponentHint(setupAssertStateOpt, exhaleHint)) =>
            MLUtil.app("AssertHint", generateAssertHintsInML(a, boogieProcAccessor, expWfRelInfo, expRelInfo))
          case _ => sys.error("assert hint has unexpected form")
        }
      case MethodCallHint(componentHints) =>
        componentHints match {
          case Seq(MethodCallStmtComponentHint(calleeName, targetVarsBpl, exhalePreHint, inhalePostHint)) =>
            val targetVarThms = MLUtil.isaToMLThms(targetVarsBpl.map(v => boogieProcAccessor.getLocalLookupDeclThm(v.asInstanceOf[LocalVar])))
            val exhalePreHintML = generateExhaleHintsInML(exhalePreHint, boogieProcAccessor, expWfRelInfo, expRelInfo).getRepresentation
            val inhalePostHintML = generateInhaleHintsInML(inhalePostHint, boogieProcAccessor, expWfRelInfo, expRelInfo)
            MLUtil.app("MethodCallHint", MLUtil.createTuple(
              Seq(MLUtil.createString(calleeName),
                targetVarThms,
                ProofGenMLConstants.inhaleRelInfoWithoutDefChecks,
                ProofGenMLConstants.exhaleRelInfoWithoutDefChecks,
                exhalePreHintML,
                inhalePostHintML))
            )
          case _ => sys.error("method call hint has unexpected form")
        }
    }
  }

  def generateStmtHintsInML(stmtProofHint: StmtProofHint, boogieProcAccessor: IsaBoogieProcAccessor, expWfRelInfo:String, expRelInfo: String) : String = {
    stmtProofHint match {
      case SeqnProofHint(hints) =>
        MLUtil.app("SeqnHint", MLUtil.createList(hints.map(hint => generateStmtHintsInML(hint, boogieProcAccessor, expWfRelInfo, expRelInfo))))
      case ScopeProofHint(_, varsBpl, bodyHint) =>
        def computeHintFromVar(localVar: LocalVar, hint: String) : String = {
          MLUtil.app("ScopeHint", MLUtil.createTuple(Seq(MLUtil.isaToMLThm(boogieProcAccessor.getLocalLookupDeclThm(localVar)), hint)))
        }

        varsBpl.foldRight(generateStmtHintsInML(bodyHint, boogieProcAccessor, expWfRelInfo, expRelInfo))(computeHintFromVar)
      case IfHint(_, componentHints) =>
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
