package viper.carbon.proofgen

import isabelle.ast._
import isabelle.ast.ProofUtil._

sealed trait StateRelInstantiation
case object SecondConjunctStateRelInst extends StateRelInstantiation
case object IdentityStateRelInst extends StateRelInstantiation

object ViperBoogieRelationIsa {
  def expressionContextType(abstractType: TypeIsa) = DataType("econtext_bpl", abstractType)

  //same as state_rel, but where evaluation and definedness state are the same (relation only takes 1 Viper state as input)
  val stateRelDefinednessSameName = "state_rel_def_same"

  val stateRelEmptyName = "state_rel_empty"

  def viperBoogieAbstractValueType(domainValueType: TypeIsa) = DataType("vbpl_absval", domainValueType)

  val viperBoogieAbstractTypeInterpId = TermIdent("vbpl_absval_ty")

  def viperBoogieAbstractTypeInterp(tyReprBpl: Term) : Term = TermApp(viperBoogieAbstractTypeInterpId, tyReprBpl)

  def trivialStateConsistency : Term = TermQuantifier(Lambda, Seq(Wildcard), BoolConst(true))

  def stateRelationDefSame( program: Term,
                     stateConsistency: Term,
                     typeRepresentation: Term,
                     translationRecord: Term,
                     auxiliaryPredicates: Term,
                     viperTotalContext: Term,
                     normalViperState: Term,
                     boogieState: Term) : Term =
    TermApp(TermIdent(stateRelDefinednessSameName), Seq(program, stateConsistency, typeRepresentation, translationRecord, auxiliaryPredicates, viperTotalContext, normalViperState, boogieState))

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

  def methodRel( stateRelEnter: Term,
               stateRelExit: Term,
               totalContextVpr: Term,
               stateConsistency: Term,
               varContextVpr: Term,
               programVpr: Term,
               expressionContextBpl: Term,
               methodDecl: Term,
               configBplEnter: Term) : Term =
    TermApp(TermIdent(methodRelName),
      Seq(stateRelEnter, stateRelExit, totalContextVpr, stateConsistency, varContextVpr, programVpr, expressionContextBpl, methodDecl, configBplEnter)
    )

  val methodRelName : String = "method_rel"

  val stmtRelPropagatePreTac : String = ruleTac("stmt_rel_propagate")
  val stmtRelPropagatePreSameRelTac : String = ruleTac("stmt_rel_propagate_same_rel")

  val stmtRelPropagatePostTac : String = ruleTac("stmt_rel_propagate_2")
  val stmtRelPropagatePostSameRelTac : String = ruleTac("stmt_rel_propagate_2_same_rel")

  val trivialConsistencyWfThm : String = "wf_total_consistency_trivial"
  val trivialConsistencyDownMonoThm : String = "true_mono_prop_downward_ord"

  def zeroMaskLookupTactic(translationRecordDefThm: String) : String =
    MLUtil.mlTacticToIsa(
      MLUtil.app("zero_mask_lookup_tac", Seq(MLUtil.contextAniquotation, MLUtil.isaToMLThm(translationRecordDefThm), "1"))
    )

  val stateRelMaskUpdateThm = "state_rel_mask_update_3"
  val zeroMaskRelThm = "zero_mask_rel_2"
  val boogieConstValSimpsThm = "boogie_const_val.simps"

  def redAssumeGoodStateTac(translationRecordDefThm: String, ctxtBplWfThm: String) = {
    MLUtil.mlTacticToIsa(
      MLUtil.app("red_assume_good_state_tac",
        Seq(MLUtil.contextAniquotation, ctxtBplWfThm, MLUtil.isaToMLThm(translationRecordDefThm), "1")
      )
    )
  }

  def progressRedBplRelTac(isaContext: String) : String =
    MLUtil.mlTacticToIsa(MLUtil.app("progress_red_bpl_rel_tac", Seq(isaContext, "1")))

  def simplifyContinuationTac(isaContext: String) : String =
    MLUtil.mlTacticToIsa(MLUtil.app("simplify_continuation", Seq(isaContext, "1")))

  def stmtRelTac(isaContext: String, stmtRelInfo: String, stmtRelTacHints: String) =
    MLUtil.mlTacticToIsa(
      MLUtil.app("stmt_rel_tac", Seq(isaContext, stmtRelInfo, stmtRelTacHints, "1")),
    )

  def postframingRelInitTac(isaContext: String, basicInfo: String, heapLookupDeclThm: String, maskLookupThyThm: String) =
    MLUtil.mlTacticToIsa(
      MLUtil.app("post_framing_rel_init_tac", Seq(isaContext, basicInfo, heapLookupDeclThm, maskLookupThyThm, "1"))
    )

  val postFramingRelAuxTrivialThm = "post_framing_rel_framing_trivial"

  val funInterpVprBplConcreteName = "fun_interp_vpr_bpl_concrete"

  def funInterpVprBplConcrete(vprProgram: Term, typeRepresentation: Term, fieldMap: Term, funMap: Term, fun: Term) : Term =
    TermApp(TermIdent(funInterpVprBplConcreteName), Seq(vprProgram, typeRepresentation, fieldMap, funMap, fun))

  val funReprConcreteName = "fun_repr_concrete"

  def funInterpVprBplWf(vprProgram: Term, typeRepresentation: Term, fieldMap: Term, funMap: Term, funInterpBpl: Term) : Term =
    TermApp(TermIdent("fun_interp_vpr_bpl_wf"), Seq(vprProgram, typeRepresentation, fieldMap, funMap, funInterpBpl))

  val funInterpVprBplConcreteWfLemmaName = "fun_interp_vpr_bpl_concrete_wf"
  val funReprConcreteInjLemmaName = "fun_repr_concrete_inj"

  val trueMonoPropDownwardOrdLemmaName = "true_mono_prop_downward_ord"
  val wfTotalConsistencyTrivialLemmaName = "wf_total_consistency_trivial"

  val defaultStateRelOptionsName = "default_state_rel_options"


  val constReprBasic = TermIdent("const_repr_basic")
  val constReprBasicInjLemmaName = "inj_const_repr_basic"
  val constReprBasicRangeLemmaName = "range_const_repr_basic"

  //order in which the Boogie constant constructors are listed when the corresponding datatype is defined
  val boogieConstDataOrder = Seq(NoPermConst, FullPermConst, NullConst, ZeroMaskConst)

  def boogieConstTy(typeRepresentation: Term, boogieConst: Term) : Term =
    TermApp(TermIdent("boogie_const_ty"), Seq(typeRepresentation, boogieConst))

  def boogieConstRel(constRepr: Term, varContext: Term, normalState: Term) : Term =
    TermApp(TermIdent("boogie_const_rel"), Seq(constRepr, varContext, normalState))

  def fieldRel(vprProgram: Term, bplVarContext: Term, fieldMap: Term, normalState: Term) : Term =
    TermApp(TermIdent("field_rel"), Seq(vprProgram, bplVarContext, fieldMap, normalState))

}

object TypeRepresentation {

  val tyReprBasicName : String = "ty_repr_basic"

  val wfTyReprBasicLemma : String = "wf_ty_repr_basic"

  private val tyReprBasic = TermIdent(tyReprBasicName)

  def makeBasicTypeRepresentation(absValInterpVpr: Term) : Term = {
    TermApp(tyReprBasic, absValInterpVpr)
  }
}

object TranslationRecord {

  val translationRecordTypeName = "tr_vpr_bpl"
  val translationRecordType = DataType(translationRecordTypeName, Seq())

  //here a record is created for the case where the well-definedness and evaluation states are the same
  def makeTranslationRecord( heapVar: Term,
                             maskVar: Term,
                             heapVarDef: Term,
                             maskVarDef: Term,
                             fieldTranslation: Term,
                             funTranslation: Term,
                             varTranslation: Term,
                             constRepr: Term,
                             labelHMTranslation: Term,
                             stateRelOptions: Term): Term =
    IsaTermUtil.makeRecord(translationRecordTypeName,
      Seq(heapVar, maskVar, heapVarDef, maskVarDef, fieldTranslation, funTranslation, varTranslation, constRepr, labelHMTranslation, stateRelOptions)
    )

  def maskVar(translationRecord: Term) : Term = TermApp(TermIdent("mask_var"), translationRecord)

}

object BoogieExpressionContext {

  val wfId = TermIdent("ctxt_wf")

  val exprContextRecordName = "econtext_bpl_general"

  def exprContextRecordType(abstractValueType: TypeIsa) : TypeIsa = DataType(exprContextRecordName, abstractValueType)

  def varContext(exprContext: Term) : Term = TermApp(TermIdent("var_context"), exprContext)

  def typeInterp(exprContext: Term) : Term = TermApp(TermIdent("type_interp"), exprContext)

  def funInterp(exprContext: Term) : Term = TermApp(TermIdent("fun_interp"), exprContext)

  def rtypeInterp(totalContext: Term) : Term = TermApp(TermIdent("rtype_interp"), totalContext)

  def wellFormed(viperProgram: Term, tyReprBpl: Term, fieldMap: Term, funMap: Term, exprContext: Term) : Term =
    TermApp(wfId, Seq(viperProgram, tyReprBpl, fieldMap, funMap, exprContext))

  def makeRecord(typeInterp: Term, varContext: Term, funInterp: Term, rtypeInterp: Term) : Term =
    IsaTermUtil.makeRecord(exprContextRecordName, Seq(typeInterp, varContext, funInterp, rtypeInterp))

}
