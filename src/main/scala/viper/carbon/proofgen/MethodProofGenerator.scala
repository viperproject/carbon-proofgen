package viper.carbon.proofgen

import viper.silver.{ast => sil}
import isabelle.ast._

import scala.collection.mutable.ListBuffer
import isabelle.ast.ProofUtil._
import isabelle.ast.IsaUtil._
import isabelle.ast.MLUtil.{isaToMLThm, isaToMLThms, mlTacticToIsa}
import viper.carbon.proofgen.functions.FunctionProofGenInterface
import viper.carbon.proofgen.hints.{IfHint, LocalVarAssignHint, MLHintGenerator, SeqnProofHint, StmtProofHint, WhileHint}


case class MethodProofGenerator(
                                 theoryName: String,
                                 vprProg: IsaViperMethodAccessor,
                                 vprTranslation: VarTranslation[sil.LocalVar],
                                 boogieProg: IsaBoogieProcAccessor,
                                 stmtProofHint: StmtProofHint,
                                 functionProofGenInterface: FunctionProofGenInterface)
{

  val globalBplData = boogieProg.globalDataAccessor

  val varContextViperName = "var_ctxt_viper"
  val varContextBoogieName = "var_ctxt_bpl"
  val typeInterpBplName = "type_interp_bpl"

  //val viperProgram = TermIdent("Pr_trivial")

  val varRelationListName = "var_relation_list_1"
  val translationRecordName = "tr_vpr_bpl_0"
  val stateRelInitialName = "state_rel_initial"

  val constReprBasic = TermIdent("const_repr_basic")

  val funReprConcrete = TermIdent("fun_repr_concrete")

  def generateProof() : Theory = {

    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    val viperVarContextDef = DefDecl(
      varContextViperName,
      Some(ArrowType(IsaTypeUtil.natType, IsaTypeUtil.optionType(ViperIsaType.viperTyType))),
      (Seq(), IsaTermUtil.mapOf(vprProg.methodArgs))
    )

    outerDecls += viperVarContextDef

    val varRelationList = {
      TermList(
        vprTranslation.availableVariables().map(
          vprVar => {
            val vprVarId = vprTranslation.translateVariableId(vprVar).get
            val bplVarId = boogieProg.getBoogieVarId(vprVar)
            TermTuple(Seq(NatConst(vprVarId), NatConst(bplVarId)))
          }
        )
      )
    }

    val varRelationListDef = DefDecl(
      varRelationListName,
      None,
      (Seq(), varRelationList)
    )

    outerDecls += varRelationListDef

    val translationRecord = TranslationRecord.makeTranslationRecord(
      heapVar = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVar = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      maskRead = TermApp(TermIdent("read_mask_concrete"), TermIdent("fun_repr_concrete")),
      maskUpdate = TermApp(TermIdent("update_mask_concrete"), TermIdent("fun_repr_concrete")),
      heapRead = TermApp(TermIdent("read_heap_concrete"), TermIdent("fun_repr_concrete")),
      heapUpdate = TermApp(TermIdent("update_heap_concrete"), TermIdent("fun_repr_concrete")),
      fieldTranslation = IsaTermUtil.mapOf(vprProg.globalDataAccessor.fieldRel),
      funTranslation = TermIdent("f_None"),
      varTranslation = IsaTermUtil.mapOf(TermIdent(varRelationListDef.name)),
      /* TODO: the constants representation should be generated by Carbon and not hardcoded */
      constRepr = constReprBasic
    )

    val translationRecordDef = DefDecl(translationRecordName,
      Some(TranslationRecord.translationRecordType),
      (Seq(), translationRecord))

    outerDecls += translationRecordDef

    outerDecls += DeclareDecl(s"${TranslationRecord.translationRecordTypeName}.defs(1)[simp]")
    outerDecls += DeclareDecl(s"${ViperIsaType.vprProgramTypeName}.defs(1)[simp]")

    val varContextBplAbbrev = AbbrevDecl(varContextBoogieName,
      None,
      (Seq(), boogieProg.varContext)
    )

    outerDecls += varContextBplAbbrev

    val stateRelInitialAbbrev: AbbrevDecl  =
      AbbrevDecl(
        stateRelInitialName,
        None,
        (Seq(TermIdent("A"), TermIdent("Pr"), TermIdent("ctxt"), TermIdent("w"), TermIdent("ns")),
          ViperBoogieRelationIsa.stateRelation(
            TermIdent("Pr"),
            TypeRepresentation.makeBasicTypeRepresentation(TermIdent("A")),
            TermIdent(translationRecordDef.name),
            TermIdent("ctxt"),
            TranslationRecord.maskVar(TermIdent(translationRecordDef.name)),
            TermIdent("w"),
            TermIdent("w"),
            TermIdent("ns")
          )
        )
      )

    outerDecls += stateRelInitialAbbrev

    val typeInterpBplAbbrev: AbbrevDecl =
      AbbrevDecl(
        typeInterpBplName,
        None,
        (Seq(TermIdent("A")), ViperBoogieRelationIsa.viperBoogieAbstractTypeInterp(TypeRepresentation.makeBasicTypeRepresentation(TermIdent("A"))))
      )

    outerDecls += typeInterpBplAbbrev

    outerDecls += mainProofLocale()

    Theory(
      theoryName = theoryName,
      importTheories = Seq(
        "TotalViper.ExpProofGenTest",
        "TotalViper.ViperBoogieTranslationInterface",
        "TotalViper.ExprWfRelML",
        "TotalViper.CPGHelperML",
        "TotalViper.StmtRelML",
        "Boogie_Lang.TypingML",
        vprProg.theoryName,
        "../"+vprProg.globalDataAccessor.theoryName,
        boogieProg.procTheoryPath
      ),
      decls = outerDecls.toSeq
    )
  }

  private def mainProofLocale() : LocaleDecl = {
    val exprContextBpl = TermIdent("ectxt")
    val totalContextVpr = TermIdent("ctxt_vpr")

    val tyInterpEqBpl = "TyInterpBpl"
    val bplCtxtWfLabel = "CtxtWf"

    val funInterpWfBpl = "WfFunBpl"

    val absvalInterpVpr = ViperTotalContext.absvalInterpTotal(totalContextVpr)

    val contextElem = ContextElem(
      //fixes
      Seq( ( exprContextBpl, ViperBoogieRelationIsa.expressionContextType(VarType("'a")) ),
           ( totalContextVpr, ViperIsaType.totalContext(VarType("'a")))
      ),

      //assumes
      Seq( (Some("VarContextBpl [simp]"), TermBinary.eq(BoogieExpressionContext.varContext(exprContextBpl), TermIdent(varContextBoogieName))),
           (Some(s"$tyInterpEqBpl [simp]"), TermBinary.eq(BoogieExpressionContext.typeInterp(exprContextBpl),
             TermApp(TermIdent(typeInterpBplName), absvalInterpVpr))
           ),
        (Some(bplCtxtWfLabel), BoogieExpressionContext.wellFormed(
           viperProgram = vprProg.globalDataAccessor.vprProgram,
           tyReprBpl = TypeRepresentation.makeBasicTypeRepresentation(absvalInterpVpr),
           fieldMap = IsaTermUtil.mapOf(vprProg.globalDataAccessor.fieldRel),
           funMap = funReprConcrete,
           exprContext = exprContextBpl
          ) ),
        (Some(funInterpWfBpl), BoogieIsaTerm.funInterpWf(
          typeInterp = BoogieExpressionContext.typeInterp(exprContextBpl),
          funDecls = globalBplData.funDecls,
          funInterp = BoogieExpressionContext.funInterp(exprContextBpl)
          )),
        (Some ("VprProgramTotal [simp]"), TermBinary.eq(ViperTotalContext.programTotal(totalContextVpr), vprProg.globalDataAccessor.vprProgram))
      )
    )

    val xId = SimpleIdentifier("x")
    val tId = SimpleIdentifier("t")

    val varContextWfBplLemma =
      LemmaDecl(
        "var_ctxt_bpl_wf",
        ContextElem.empty(),
        TermQuantifier(All, Seq(xId, tId),
          TermBinary.implies(
            TermBinary.eq(BoogieIsaTerm.lookupVarTy(BoogieExpressionContext.varContext(exprContextBpl), TermIdent(xId)), IsaTermUtil.some(TermIdent(tId))),
            BoogieIsaTerm.wfTy(TermIdent(tId))
          )
        ),
        Proof(Seq((using(boogieProg.varContextWfThm, byTac(simp)) : String)))
      )

    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty
    outerDecls += varContextWfBplLemma

    val lookupVarRelTac = "lookup_var_rel_tac"
    val simpWithTrDef = "simp_with_tr_def_tac"
    val simpWithTyReprDef = "simp_with_ty_repr_def_tac"
    val typeSafetyThmMap = "type_safety_thm_map"
    val lookupVarBplThms = "lookup_var_bpl_thms"
    val lookupFunBplThms = "lookup_fun_bpl_thms"

    val expRelInfo = "exp_rel_info"
    val expWfRelInfo = "exp_wf_rel_info"

    val basicStmtRelInfo = "basic_stmt_rel_info"
    val stmtRelInfo = "stmt_rel_info"
    val stmtRelHints = "stmt_rel_hints"

    val heapReadWfTac = "heap_read_wf_tac"
    val heapReadMatchTac = "heap_read_match_tac"
    val fieldRelTac  = "field_rel_tac"
    val fieldLookupTac = "field_lookup_tac"

    val fieldAccInitTac = "field_acc_init_tac"
    val fieldAccessWfRelTacAuxInst = "field_access_wf_rel_tac_aux_inst"

    val mlInitializationCode =
      Seq(
        MLUtil.defineVal(lookupVarRelTac, MLUtil.simpAsmSolved(isaToMLThms(Seq(definitionLemmaFromName(translationRecordName), definitionLemmaFromName(varRelationListName))))),
        MLUtil.defineVal(simpWithTrDef, MLUtil.simpAsmSolved(isaToMLThms(Seq(definitionLemmaFromName(translationRecordName))))),
        MLUtil.defineVal(simpWithTyReprDef, MLUtil.simpAsmSolved(isaToMLThms(Seq(definitionLemmaFromName(TypeRepresentation.tyReprBasicName))))),
        MLUtil.defineVal(typeSafetyThmMap, ViperBoogieMLUtil.genTypeSafetyThmMap(
          isaToMLThm(funInterpWfBpl),
          isaToMLThm(globalBplData.funDeclsWf.toString),
          isaToMLThm(varContextWfBplLemma.name),
          isaToMLThm(ViperBoogieRelationIsa.stateRelationWellTypedThm.toString)
          )
        ),

        //TODO: more fine-grained approach (specify required lookup theorems for different expressions)
        // TODO: take lookup theorems for constants and globals into account
        MLUtil.defineVal(lookupVarBplThms, isaToMLThms(
          /** include all Boogie variable lookup theorems that are required for Boogie expressions corresponding to Viper
            * expressions
            */
          Seq(
            boogieProg.getLookupThyThm(HeapGlobalVar),
            boogieProg.getLookupThyThm(MaskGlobalVar)
          ) ++
          boogieProg.getAllLocalVariables().map(l => boogieProg.getLookupThyThm(l)).toSeq ++
          vprProg.origProgram.fields.map(field => boogieProg.getLookupThyThm(FieldConst(field)))
        )),

        MLUtil.defineVal(lookupFunBplThms, isaToMLThms(
          functionProofGenInterface.allFunBplLookupLemmasForViperExpressions(vprProg.origProgram.functions)
        )),

        MLUtil.defineFun(heapReadWfTac, Seq("ctxt"),
          MLUtil.seqPrimeTac(
            MLUtil.app(MLUtil.simpAsm(MLUtil.isaToMLThms(Seq(definitionLemmaFromName(translationRecordName)))), "ctxt"),
            MLUtil.resolveTac("ctxt", MLUtil.isaToMLThms(Seq("heap_wf_concrete[OF wf_ty_repr_basic CtxtWf]")))
          )
        ),

        MLUtil.defineFun(heapReadMatchTac, Seq("ctxt"),
            MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
              definitionLemmaFromName(translationRecordName),
              definitionLemmaFromName(TypeRepresentation.tyReprBasicName),
              definitionLemmaFromName("read_heap_concrete"))
            ), "ctxt"),
          ),

        MLUtil.defineFun(fieldRelTac, Seq("ctxt"),
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
            definitionLemmaFromName(translationRecordName),
            definitionLemmaFromName(vprProg.globalDataAccessor.fieldRel.toString)
            )),
            "ctxt"
          )
        ),

        MLUtil.defineFun(fieldLookupTac, Seq("ctxt"),
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
            definitionLemmaFromName(vprProg.globalDataAccessor.vprProgram.toString),
            definitionLemmaFromName(vprProg.globalDataAccessor.fields.toString)
          )),
            "ctxt")
        ),

        MLUtil.defineVal(expRelInfo, ViperBoogieMLUtil.createExpRelInfo(
          typeSafetyThmMap,
          lookupVarRelTac,
          simpWithTrDef,
          lookupVarThms = lookupVarBplThms,
          lookupFunBplThms = lookupFunBplThms,
          fieldAccessRelPreTac =
            ViperBoogieMLUtil.fieldAccessRelPreTac(
              heapReadWfTac = heapReadWfTac,
              heapReadMatchTac = heapReadMatchTac,
              fieldRelTac = fieldRelTac,
              fieldLookupTac = fieldLookupTac
            )
        )),

        MLUtil.defineFun(fieldAccInitTac, Seq("ctxt"), MLUtil.resolveTac("ctxt",
          MLUtil.isaToMLThms(Seq(ProofUtil.OF("syn_field_access_wf_rel", Seq(bplCtxtWfLabel, "wf_ty_repr_basic"))))
        )),

        MLUtil.defineVal(fieldAccessWfRelTacAuxInst,
          ViperBoogieMLUtil.fieldAccessWfRelTacAuxInst(
            fieldAccInitTac = fieldAccInitTac,
            lookupMaskVarTac = simpWithTrDef,
            fieldRelTac = fieldRelTac,
            fieldLookupTac = fieldLookupTac,
            tyArgsEqTac =  simpWithTyReprDef,
            expRelInfo = expRelInfo
          )
        ),

        MLUtil.defineVal(expWfRelInfo,
          ViperBoogieMLUtil.createExpWfRelInfo(fieldAccessWfRelTacAuxInst)
          ),

        MLUtil.defineVal(basicStmtRelInfo, ViperBoogieMLUtil.createBasicStmtRelInfo(
          isaToMLThm(bplCtxtWfLabel),
          isaToMLThm(definitionLemmaFromName(translationRecordName)),
          lookupVarRelTac,
          "assm_full_simp_solved_with_thms_tac " + isaToMLThms(Seq(definitionLemmaFromName(varContextViperName))),
          MLUtil.isaToMLThm(tyInterpEqBpl)
          )
        ),

        MLUtil.defineVal(stmtRelInfo, ViperBoogieMLUtil.createStmtRelInfo(
          basicStmtRelInfo = basicStmtRelInfo,
          atomicRelTac = "atomic_rel_inst_tac"
        )),

        MLUtil.defineVal(stmtRelHints, MLHintGenerator.generateHintsInML(stmtProofHint, boogieProg, expWfRelInfo, expRelInfo))
      )

    outerDecls += MLDecl(mlInitializationCode, MLNormal)

    val mainTheorem = LemmaDecl("method_rel_proof",
      ContextElem.empty(),
      ViperBoogieRelationIsa.stmtRel(
        stateRelEnter=ViperBoogieRelationIsa.stateRelEmpty(TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, vprProg.globalDataAccessor.vprProgram, exprContextBpl))),
        stateRelExit=TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, vprProg.globalDataAccessor.vprProgram, exprContextBpl)),
        totalContextVpr=totalContextVpr,
        stateConsistency=TermIdent("StateCons"),
        varContextVpr=TermIdent(varContextViperName),
        programVpr=TermIdent("P"),
        expressionContextBpl=exprContextBpl,
        stmtVpr= vprProg.methodBody,
        configBplEnter=ViperIsaTerm.convertAstToProgramPoint(TermIdent(boogieProg.procBodyAstDef)),
        configBplExit=BoogieIsaTerm.finalProgramPoint),
      Proof(
        initBoogieStateProof(bplCtxtWfLabel) ++
        mainProof(stmtRelInfo, stmtRelHints) ++
        Seq(doneTac)
      )
    )

    outerDecls += mainTheorem

    LocaleDecl("method_proof", contextElem, outerDecls.toSeq)
  }

  private def mainProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    Seq(
      applyTac(unfoldTac(IsaUtil.definitionLemmaFromName(vprProg.methodBody.toString))),
      applyTac(ViperBoogieRelationIsa.stmtRelPropagatePostSameRelTac),
      applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
      applyTac(ViperBoogieRelationIsa.progressBplTac(MLUtil.contextAniquotation)),
    )
  }

  private def initBoogieStateProof(ctxtBplWfThm: String): Seq[String] = {
    Seq(
      applyTac(unfoldTac(Seq(IsaUtil.definitionLemmaFromName(ViperBoogieRelationIsa.stateRelEmptyName),
                             IsaUtil.definitionLemmaFromName(boogieProg.procBodyAstDef)))),
      applyTac(BoogieIsaTerm.simplifyAstToProgramPointTac),
      applyTac(ViperBoogieRelationIsa.stmtRelPropagatePreTac),
      applyTac(ruleTac("exI")),
      applyTac(BoogieIsaTerm.unfoldASTBlockInGoalTac),
      applyTac(BoogieIsaTerm.redAstPropagateRelTac),
      applyTac(BoogieIsaTerm.redAstOneSimpleCmdTac),
      applyTac(BoogieIsaTerm.assignIntroAltTac),
      applyTac(simpTac(boogieProg.getLookupThyThm(MaskGlobalVar))),
      applyTac(ruleTac(BoogieIsaTerm.redVarThm)),
      applyTac(ViperBoogieRelationIsa.zeroMaskLookupTactic(IsaUtil.definitionLemmaFromName(translationRecordName))),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName))),
    ) ++
      initUpdatedStateInRelation() ++
      initAssumeGoodState(ctxtBplWfThm)
  }

  private def initUpdatedStateInRelation() : Seq[String] = {
    Seq(
      applyTac(ruleTac(ViperBoogieRelationIsa.stateRelMaskUpdateThm)),
      applyTac(fastforceTac),
      applyTac(simpTac),
      applyTac(ruleTac(ViperBoogieRelationIsa.zeroMaskRelThm)),
      applyTac(simpTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecordName))),
      applyTac(simpTac),
      applyTac(simpTac)
    )
  }

  private def initAssumeGoodState(ctxtBplWfThm: String) : Seq[String] = {
    Seq(
      applyTac(BoogieIsaTerm.redAstPropagateRelTac),
      applyTac(BoogieIsaTerm.redAstOneSimpleCmdTac),
      applyTac(ViperBoogieRelationIsa.redAssumeGoodStateTac(IsaUtil.definitionLemmaFromName(translationRecordName),
        MLUtil.isaToMLThm(ctxtBplWfThm)
        )),
      applyTac(assumeTac),
      applyTac(ruleTac("conjI")),
      applyTac(simpTac),
      applyTac(BoogieIsaTerm.redAstReflTac),
      applyTac(assumeTac)
    )
  }

}
