package viper.carbon.proofgen

import isabelle.ast._
import isabelle.ast.ProofUtil._

object ViperBoogieRelationIsa {
  def expressionContextType(abstractType: TypeIsa) = DataType("econtext_bpl", abstractType)

  val stateRelDefSameName = "state_rel_def_same"
  val stateRelEmptyName = "state_rel_empty"

  val viperBoogieAbstractTypeInterpId = TermIdent("vbpl_absval_ty")

  def viperBoogieAbstractTypeInterp(tyReprBpl: Term) : Term = TermApp(viperBoogieAbstractTypeInterpId, tyReprBpl)

  def stateRelationDefSame(program: Term,
                           typeRepresentation: Term,
                           translationRecord: Term,
                           auxiliaryPredicates: Term,
                           viperTotalContext: Term,
                           normalViperState: Term,
                           boogieState: Term) : Term =
    TermApp(TermIdent(stateRelDefSameName), Seq(program, typeRepresentation, translationRecord, auxiliaryPredicates, viperTotalContext, normalViperState, boogieState))

  def stateRelationWellTypedThm = TermIdent("state_rel_state_well_typed")

  def stateRelEmpty(stateRel: Term) = TermApp(TermIdent(stateRelEmptyName), stateRel)

  def stmtRel( stateRelEnter: Term,
               stateRelExit: Term,
               totalContextVpr: Term,
               stateConsistency: Term,
               varContextVpr: Term,
               programVpr: Term,
               expressionContextBpl: Term,
               stmtVpr: Term,
               configBplEnter: Term,
               configBplExit: Term) : Term =
    TermApp(TermIdent("stmt_rel"),
      Seq(stateRelEnter, stateRelExit, totalContextVpr, stateConsistency, varContextVpr, programVpr, expressionContextBpl, stmtVpr, configBplEnter, configBplExit)
    )

  val stmtRelPropagatePreTac : String = ruleTac("stmt_rel_propagate")
  val stmtRelPropagatePreSameRelTac : String = ruleTac("stmt_rel_propagate_same_rel")

  val stmtRelPropagatePostTac : String = ruleTac("stmt_rel_propagate_2")
  val stmtRelPropagatePostSameRelTac : String = ruleTac("stmt_rel_propagate_2_same_rel")

  def zeroMaskLookupTactic(translationRecordDefThm: String) : String =
    MLUtil.mlTacticToIsa(
      MLUtil.app("zero_mask_lookup_tac", Seq(MLUtil.contextAniquotation, MLUtil.isaToMLThm(translationRecordDefThm), "1"))
    )

  val stateRelMaskUpdateThm = "state_rel_mask_update_2"
  val zeroMaskRelThm = "zero_mask_rel_2"

  def redAssumeGoodStateTac(translationRecordDefThm: String, ctxtBplWfThm: String) = {
    MLUtil.mlTacticToIsa(
      MLUtil.app("red_assume_good_state_tac",
        Seq(MLUtil.contextAniquotation, ctxtBplWfThm, MLUtil.isaToMLThm(translationRecordDefThm), "1")
      )
    )
  }

  def progressBplTac(isaContext: String) : String =
    MLUtil.mlTacticToIsa(MLUtil.app("progress_tac", Seq(isaContext, "1")))

  def stmtRelTac(isaContext: String, stmtRelInfo: String, stmtRelTacHints: String) =
    MLUtil.mlTacticToIsa(
      MLUtil.app("stmt_rel_tac", Seq(isaContext, stmtRelInfo, stmtRelTacHints, "1")),
    )
}

case object TypeRepresentation {

  val tyReprBasicName : String = "ty_repr_basic"

  private val tyReprBasic = TermIdent(tyReprBasicName)

  def makeBasicTypeRepresentation(absValInterpVpr: Term) : Term = {
    TermApp(tyReprBasic, absValInterpVpr)
  }
}

case object TranslationRecord {
  val translationRecordTypeName = "tr_vpr_bpl"
  val translationRecordType = DataType(translationRecordTypeName, Seq())

  //here a record is created for the case where the well-definedness and evaluation states are the same
  def makeTranslationRecord( heapVar: Term,
                             maskVar: Term,
                             maskRead: Term,
                             maskUpdate: Term,
                             heapRead: Term,
                             heapUpdate: Term,
                             fieldTranslation: Term,
                             funTranslation: Term,
                             varTranslation: Term,
                             constRepr: Term): Term =
    IsaTermUtil.makeRecord(translationRecordTypeName,
      Seq(heapVar, maskVar,  //evaluation state
          heapVar, maskVar,  //well-def state
          maskRead, maskUpdate, heapRead, heapUpdate, fieldTranslation, funTranslation, varTranslation, constRepr)
    )

  def maskVar(translationRecord: Term) : Term = TermApp(TermIdent("mask_var"), translationRecord)

}

case object BoogieExpressionContext {

  val wfId = TermIdent("ctxt_wf")

  def varContext(exprContext: Term) : Term = TermApp(TermIdent("var_context"), exprContext)

  def typeInterp(exprContext: Term) : Term = TermApp(TermIdent("type_interp"), exprContext)

  def funInterp(exprContext: Term) : Term = TermApp(TermIdent("fun_interp"), exprContext)

  def wellFormed(viperProgram: Term, tyReprBpl: Term, fieldMap: Term, funMap: Term, exprContext: Term) =
    TermApp(wfId, Seq(viperProgram, tyReprBpl, fieldMap, funMap, exprContext))

}

