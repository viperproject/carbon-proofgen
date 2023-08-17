package viper.carbon.proofgen

import isabelle.ast
import viper.silver.{ast => sil}
import isabelle.ast._

import scala.collection.mutable.ListBuffer
import isabelle.ast.ProofUtil._
import isabelle.ast.IsaUtil._
import isabelle.ast.MLUtil.{isaToMLThm, isaToMLThms, mlTacticToIsa, simpAsm, simpOnly}
import viper.carbon.boogie.LocalVar
import viper.carbon.modules.impls.{HeapStateComponent, PermissionStateComponent}
import viper.carbon.proofgen.functions.FunctionProofGenInterface
import viper.carbon.proofgen.hints.{AtomicHint, ExhaleStmtComponentHint, ExhaleStmtHint, IfHint, InhaleProofHint, InhaleStmtComponentHint, InhaleStmtHint, LocalVarAssignHint, MLHintGenerator, MethodProofHint, ResetStateComponentHint, SeqnProofHint, StateProofHint, StmtProofHint, WhileHint}


case class MethodProofGenerator(
                                 theoryName: String,
                                 methodAccessor: IsaViperMethodAccessor,
                                 progAccessor: IsaViperGlobalDataAccessor,
                                 vprTranslation: VarTranslation[sil.LocalVar],
                                 boogieProg: IsaBoogieProcAccessor,
                                 methodProofHint: MethodProofHint,
                                 functionProofGenInterface: FunctionProofGenInterface)
{

  val globalBplData = boogieProg.globalDataAccessor

  val varContextViperName = "var_ctxt_viper"
  val varContextBoogieName = "var_ctxt_bpl"
  val typeInterpBplName = "type_interp_bpl"

  val varRelationListName = "var_relation_list_1"
  val varRelationBoundedByName = "var_relation_list_1_bound"
  val translationRecordName = "tr_vpr_bpl_0"
  val stateRelInitialName = "state_rel_initial"

  val constReprBasic = TermIdent("const_repr_basic")

  val funReprConcrete = TermIdent("fun_repr_concrete")

  def generateProof() : Theory = {

    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    val viperVarContextDef = DefDecl(
      varContextViperName,
      Some(ArrowType(IsaTypeUtil.natType, IsaTypeUtil.optionType(ViperIsaType.viperTyType))),
      (Seq(), IsaTermUtil.mapOf(IsaTermUtil.appendList(methodAccessor.methodArgs, methodAccessor.methodRets)))
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
      Some(IsaTypeUtil.listType(TupleType(ViperIsaType.varNameType, BoogieIsaType.varNameType))),
      (Seq(), varRelationList)
    )

    outerDecls += varRelationListDef

    val varRelationBoundedBy = LemmaDecl(
      varRelationBoundedByName,
      ViperBoogieIsaUtil.allVarsInListBoundedBy(
        TermIdent(varRelationListName),
        ViperBoogieIsaUtil.maxInRangeOfList(varRelationList)
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(varRelationListName)))))
    )

    outerDecls += varRelationBoundedBy

    val translationRecord = TranslationRecord.makeTranslationRecord(
      heapVar = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVar = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      heapVarDef = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVarDef = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      fieldTranslation = IsaTermUtil.mapOf(progAccessor.fieldRel),
      funTranslation = TermIdent("f_None"),
      varTranslation = IsaTermUtil.mapOf(TermIdent(varRelationListDef.name)),
      /* TODO: the constants representation should be generated by Carbon and not hardcoded */
      constRepr = constReprBasic,
      stateRelOptions = TermIdent("default_state_rel_options")
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
          ViperBoogieRelationIsa.stateRelationDefSame(
            TermIdent("Pr"),
            ViperBoogieRelationIsa.trivialStateConsistency,
            TypeRepresentation.makeBasicTypeRepresentation(TermIdent("A")),
            TermIdent(translationRecordDef.name),
            IsaTermUtil.emptyMap,
            TermIdent("ctxt"),
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
        "TotalViper.ViperBoogieTranslationInterface",
        "TotalViper.ExprWfRelML",
        "TotalViper.CPGHelperML",
        "TotalViper.StmtRelML",
        "TotalViper.ViperBoogieEndToEndML",
        "Boogie_Lang.TypingML",
        "../"+progAccessor.theoryName,
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

    val vprProgramTotal = "VprProgramTotal"

    val rtypeInterpEmpty = "RtypeInterpEmpty"

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
           viperProgram = progAccessor.vprProgram,
           tyReprBpl = TypeRepresentation.makeBasicTypeRepresentation(absvalInterpVpr),
           fieldMap = IsaTermUtil.mapOf(progAccessor.fieldRel),
           funMap = funReprConcrete,
           exprContext = exprContextBpl
          ) ),
        (Some(funInterpWfBpl), BoogieIsaTerm.funInterpWf(
          typeInterp = BoogieExpressionContext.typeInterp(exprContextBpl),
          funDecls = globalBplData.funDecls,
          funInterp = BoogieExpressionContext.funInterp(exprContextBpl)
          )),
        (Some ("VprProgramTotal [simp]"), TermBinary.eq(ViperTotalContext.programTotal(totalContextVpr), progAccessor.vprProgram)),
        (Some(rtypeInterpEmpty + "[simp]"), TermBinary.eq(ViperTotalContext.rtypeInterp(exprContextBpl), TermList(Seq())))
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

    val basicDisjointnessLemmas = LemmasDecl(
      "basic_disjointness_lemmas",
      Seq(
        not_satisfies_prop_set(ProofUtil.OF("list_all_ran_map_of", varRelationBoundedByName)),
        not_satisfies_prop_set(ProofUtil.OF("list_all_ran_map_of", progAccessor.fieldRelBoundedLemma)),
        not_satisfies_prop_set("const_repr_basic_bound_2")
      )
    )

    outerDecls += basicDisjointnessLemmas

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
    val stmtRelInfoWithoutDefChecks = "stmt_rel_info_opt"
    val stmtRelHints = "stmt_rel_hints"
    val stmtInhalePreconditionHints = "stmt_precondition_hints"
    val stmtPostconditionFramingHints = "stmt_postcondition_framing_hints"
    val stmtExhalePostconditionHints = "stmt_postcondition_hints"

    val stmtPreconditionHintValue =
      if(methodAccessor.origMethod.pres.isEmpty) {
        None
      } else {
        Some(AtomicHint(InhaleStmtHint(Seq(InhaleStmtComponentHint(methodProofHint.preconditionInhaleHint.conjoinBodyHints)))))
      }

    val stmtPostconditionFramingValue =
      if(methodAccessor.origMethod.posts.isEmpty) {
        None
      } else {
        Some(AtomicHint(InhaleStmtHint(Seq(InhaleStmtComponentHint(methodProofHint.postconditionFramingHint._2.conjoinBodyHints)))))
      }

    val stmtPostconditionHintValue =
      if(methodAccessor.origMethod.posts.isEmpty) {
        None
      } else {
        Some(AtomicHint(ExhaleStmtHint(Seq(ExhaleStmtComponentHint(methodProofHint.postconditionExhaleHint.conjoinBodyHints)))))
      }

    val inhaleRelInfo = "inhale_rel_info"
    val inhaleRelInfoWithoutDefChecks = "inhale_rel_info_opt"

    val exhaleRelInfo = "exhale_rel_info"
    val exhaleRelInfoWithoutDefChecks = "exhale_rel_info_opt"

    val auxVarDisjTac = "aux_var_disj_tac"

    val heapReadWfTac = "heap_read_wf_tac"
    val heapReadMatchTac = "heap_read_match_tac"
    val fieldRelTac  = "field_rel_tac"
    val fieldLookupTac = "field_lookup_tac"
    val fieldRelSingleTac = "field_rel_single_tac"

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
            boogieProg.getGlobalLookupTyThm(HeapGlobalVar),
            boogieProg.getGlobalLookupTyThm(MaskGlobalVar)
          ) ++
          boogieProg.getAllLocalVariables().map(l => boogieProg.getLocalLookupTyThm(l)).toSeq ++
          methodAccessor.origProgram.fields.map(field => boogieProg.getGlobalLookupTyThm(FieldConst(field)))
        )),

        MLUtil.defineVal(lookupFunBplThms, isaToMLThms(
          functionProofGenInterface.allFunBplLookupLemmasForViperExpressions(methodAccessor.origProgram.functions)
        )),

        MLUtil.defineFun(heapReadWfTac, Seq("ctxt"),
          MLUtil.seqPrimeTac(
            MLUtil.app(MLUtil.simpAsm(MLUtil.isaToMLThms(Seq(definitionLemmaFromName(translationRecordName)))), "ctxt"),
            MLUtil.resolveTac("ctxt", MLUtil.isaToMLThms(Seq("heap_wf_concrete[OF CtxtWf wf_ty_repr_basic]")))
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
            definitionLemmaFromName(progAccessor.fieldRel.toString)
            )),
            "ctxt"
          )
        ),

        MLUtil.defineFun(fieldLookupTac, Seq("ctxt"),
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
            definitionLemmaFromName(progAccessor.vprProgram.toString),
            definitionLemmaFromName(progAccessor.fields.toString)
          )),
            "ctxt")
        ),

        MLUtil.defineVal(fieldRelSingleTac,
          MLUtil.app("field_rel_single_inst_tac", Seq(fieldRelTac, fieldLookupTac))
        ),

        MLUtil.defineVal(expRelInfo, ViperBoogieMLUtil.createExpRelInfo(
          typeSafetyThmMap,
          lookupVarRelTac,
          simpWithTrDef,
          lookupVarThms = lookupVarBplThms,
          lookupFunBplThms = lookupFunBplThms,
          simplifyRtypeInterpTac =
            s"fn ctxt => ${MLUtil.tryPrimeTac("("+MLUtil.simpOnly(MLUtil.isaToMLThms(Seq(rtypeInterpEmpty)), "ctxt") +")")}",
          fieldAccessRelPreTac =
            ViperBoogieMLUtil.fieldAccessRelPreTac(
              heapReadWfTac = heapReadWfTac,
              heapReadMatchTac = heapReadMatchTac,
              fieldRelSingleTac = fieldRelSingleTac
            )
        )),

        MLUtil.defineFun(fieldAccInitTac, Seq("ctxt"), MLUtil.resolveTac("ctxt",
          MLUtil.isaToMLThms(Seq(ProofUtil.OF("syn_field_access_valid_wf_rel", Seq(bplCtxtWfLabel, "wf_ty_repr_basic"))))
        )),

        MLUtil.defineVal(fieldAccessWfRelTacAuxInst,
          ViperBoogieMLUtil.fieldAccessWfRelTacAuxInst(
            fieldAccInitTac = fieldAccInitTac,
            lookupMaskVarTac = simpWithTrDef,
            fieldRelSingleTac = fieldRelSingleTac,
            tyArgsEqTac =  simpWithTyReprDef,
            expRelInfo = expRelInfo
          )
        ),

        MLUtil.defineVal(expWfRelInfo,
          ViperBoogieMLUtil.createExpWfRelInfo(fieldAccessWfRelTacAuxInst)
        ),

        MLUtil.defineVal(auxVarDisjTac,
          //map_upd_set_dom for the method call case
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(definitionLemmaFromName(translationRecordName), basicDisjointnessLemmas.name, "map_upd_set_dom")))
        ),

        MLUtil.defineVal(basicStmtRelInfo, ViperBoogieMLUtil.createBasicStmtRelInfo(
          ctxtWfThm = isaToMLThm(bplCtxtWfLabel),
          vprProgramContextEqThm = isaToMLThm(vprProgramTotal),
          trDefThm = isaToMLThm(definitionLemmaFromName(translationRecordName)),
          varRelTac = lookupVarRelTac,
          varContextVprTac = "assm_full_simp_solved_with_thms_tac " + isaToMLThms(Seq(definitionLemmaFromName(varContextViperName))),
          fieldRelSingleTac = fieldRelSingleTac,
          auxVarDisjTac = auxVarDisjTac,
          tyInterpEContextBplEq = MLUtil.isaToMLThm(tyInterpEqBpl)
          )
        ),

        MLUtil.defineVal(
          inhaleRelInfo,
          ViperBoogieMLUtil.createInhaleRelInfo(
            basicStmtRelInfo = basicStmtRelInfo,
            atomicInhaleRelTac = "atomic_inhale_rel_inst_tac",
            isInhRelInvThm = MLUtil.isaToMLThm(InhaleRelUtil.isInhRelInvThm(true)),
            noDefChecksTacOpt = InhaleRelUtil.inhNoDefChecksTacOpt(true),
          )
        ),

        MLUtil.defineVal(
          inhaleRelInfoWithoutDefChecks,
          ViperBoogieMLUtil.createInhaleRelInfo(
            basicStmtRelInfo = basicStmtRelInfo,
            atomicInhaleRelTac = "atomic_inhale_rel_inst_tac",
            isInhRelInvThm = MLUtil.isaToMLThm(InhaleRelUtil.isInhRelInvThm(false)),
            noDefChecksTacOpt = InhaleRelUtil.inhNoDefChecksTacOpt(false),
          )
        ),

        MLUtil.defineVal(
          exhaleRelInfo,
          ViperBoogieMLUtil.createExhaleRelInfo(
            basicStmtRelInfo = basicStmtRelInfo,
            atomicExhaleRelTac = "atomic_exhale_rel_inst_tac",
            isExhRelInvThm = MLUtil.isaToMLThm(ExhaleRelUtil.isExhRelInvThm(true)),
            noDefChecksTacOpt = ExhaleRelUtil.exhNoDefChecksTacOpt(true)
          )
        ),

        MLUtil.defineVal(
          exhaleRelInfoWithoutDefChecks,
          ViperBoogieMLUtil.createExhaleRelInfo(
            basicStmtRelInfo = basicStmtRelInfo,
            atomicExhaleRelTac = "atomic_exhale_rel_inst_tac",
            isExhRelInvThm = MLUtil.isaToMLThm(ExhaleRelUtil.isExhRelInvThm(false)),
            noDefChecksTacOpt = ExhaleRelUtil.exhNoDefChecksTacOpt(false)
          )
        ),

        MLUtil.defineVal(stmtRelInfo, ViperBoogieMLUtil.createStmtRelInfo(
          basicStmtRelInfo = basicStmtRelInfo,
          atomicRelTac = "atomic_rel_inst_tac",
          inhaleRelInfo = inhaleRelInfo,
          exhaleRelInfo = exhaleRelInfo
        )),

        MLUtil.defineVal(stmtRelInfoWithoutDefChecks, ViperBoogieMLUtil.createStmtRelInfo(
          basicStmtRelInfo = basicStmtRelInfo,
          atomicRelTac = "atomic_rel_inst_tac",
          inhaleRelInfo = inhaleRelInfo, //TODO: support optimized inhale
          exhaleRelInfo = exhaleRelInfoWithoutDefChecks
        )),

        MLUtil.defineVal(stmtRelHints, MLHintGenerator.generateStmtHintsInML(methodProofHint.bodyHint, boogieProg, expWfRelInfo, expRelInfo)),
      ) ++
      stmtPreconditionHintValue.fold[Seq[String]](Seq())(h => Seq(
          MLUtil.defineVal(stmtInhalePreconditionHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, expWfRelInfo, expRelInfo))
      )) ++
      stmtPostconditionFramingValue.fold[Seq[String]](Seq())(h => Seq(
        MLUtil.defineVal(stmtPostconditionFramingHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, expWfRelInfo, expRelInfo))
      )) ++
      stmtPostconditionHintValue.fold[Seq[String]](Seq())(h =>
        Seq (
          MLUtil.defineVal(stmtExhalePostconditionHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, expWfRelInfo, expRelInfo)),
        )
      )

    outerDecls += MLDecl(mlInitializationCode, MLNormal)

    val outputStateRel = TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, progAccessor.vprProgram, exprContextBpl))

    val mainTheorem = LemmaDecl("method_rel_proof",
      ContextElem.empty(),
      ViperBoogieRelationIsa.methodRel(
        stateRelEnter=ViperBoogieRelationIsa.stateRelEmpty(TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, progAccessor.vprProgram, exprContextBpl))),
        stateRelExit=outputStateRel,
        totalContextVpr=totalContextVpr,
        stateConsistency=ViperBoogieRelationIsa.trivialStateConsistency,
        varContextVpr=TermIdent(varContextViperName),
        programVpr=TermIdent("P"),
        expressionContextBpl=exprContextBpl,
        methodDecl = methodAccessor.methodDecl,
        configBplEnter=ViperIsaTerm.convertAstToProgramPoint(TermIdent(boogieProg.procBodyAstDef))
      ),
      Proof(
        Seq(
          applyTac(unfoldTac(IsaUtil.definitionLemmaFromName(ViperBoogieRelationIsa.methodRelName))),
          applyTac(ruleTac("exI")),
          applyTac(introTac("conjI")),
        ) ++
        initBoogieStateProof(bplCtxtWfLabel, IsaPrettyPrinter.prettyPrint(outputStateRel)) ++
        inhalePreconditionProof(stmtRelInfo, stmtInhalePreconditionHints) ++
        postconditionFramingProof(methodProofHint.postconditionFramingHint._1, basicStmtRelInfo, stmtRelInfo, stmtPostconditionFramingHints) ++
        methodBodyProof(stmtRelInfo, stmtRelHints) ++
        exhalePostconditionProof(stmtRelInfoWithoutDefChecks, stmtExhalePostconditionHints) ++
        Seq(doneTac)
      )
    )

    outerDecls += mainTheorem

    LocaleDecl("method_proof", contextElem, outerDecls.toSeq)
  }

  private def inhalePreconditionProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    applyTac(unfoldTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPrecondition))) +:
    (
      if(methodAccessor.origMethod.pres.isEmpty) {
        Seq(
          applyTac(ruleTac("inhale_stmt_rel_no_inv")), //TODO: do not hardcode theorem names
          applyTac(ruleTac("inhale_rel_true"))
        )
      } else {
        Seq(
          applyTac(ViperBoogieRelationIsa.stmtRelPropagatePostSameRelTac),
          applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
          applyTac(ViperBoogieRelationIsa.progressBplRelTac(MLUtil.contextAniquotation)),
        )
      }
    )
  }

  private def exhalePostconditionProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    applyTac(unfoldTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPostcondition))) +:
      (
        if(methodAccessor.origMethod.posts.isEmpty) {
          Seq(
            applyTac(ruleTac(ExhaleRelUtil.stmtRelExhaleTrueThm)),
          )
        } else {
          Seq(
            //applyTac(ViperBoogieRelationIsa.stmtRelPropagatePostSameRelTac),
            applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
           // applyTac(ViperBoogieRelationIsa.progressBplRelTac(MLUtil.contextAniquotation)),
          )
        }
      )
  }

  private def postconditionFramingProof(setupStateHint: Seq[StateProofHint], basicInfo: String, stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    if(methodAccessor.origMethod.posts.isEmpty) {
      Nil
    } else {
      setupStateHint match {
        case Seq(ResetStateComponentHint(HeapStateComponent, Seq(heapVar : LocalVar)), ResetStateComponentHint(PermissionStateComponent, Seq(maskVar : LocalVar))) =>{
          val initTac =
            ViperBoogieRelationIsa.postframingRelInitTac(
              MLUtil.contextAniquotation,
              basicInfo,
              MLUtil.isaToMLThm(boogieProg.getLocalLookupDeclThm(heapVar)),
              MLUtil.isaToMLThm(boogieProg.getLocalLookupTyThm(maskVar))
            )

          Seq(
            applyTac(initTac),
            applyTac(ProofUtil.simpTacOnly(Seq(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPostcondition)))),
            applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints))
          )
        }

        case _ => sys.error("The initial state setup for postcondition framing is not supported by proof generation.")
      }
    }
  }

  private def methodBodyProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    Seq(
      applyTac(ruleTac("impI")),
      applyTac("(rule exI)+"),
      applyTac(introTac("conjI")),
      applyTac(simpTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody))),
      applyTac(ViperBoogieRelationIsa.stmtRelPropagatePostSameRelTac)
    ) ++ (if(!methodAccessor.origMethod.posts.isEmpty) {
      //need to first progress the non-deterministic if-statement that contains the framedness encoding of the postcondition
      Seq(
        applyTac(ViperBoogieRelationIsa.stmtRelPropagatePreSameRelTac),
        applyTac(BoogieIsaTerm.redAstBplRelTransitiveTac),
        applyTac(BoogieIsaTerm.redAstBplRelIfNondetToElseBranchTac),
        applyTac(ViperBoogieRelationIsa.simplifyContinuationTac(MLUtil.contextAniquotation)),
        applyTac(ViperBoogieRelationIsa.progressBplRelTac(MLUtil.contextAniquotation)) //will progress the empty block
      )
    } else {
      Nil
    }) ++ Seq(
      applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
      applyTac(ViperBoogieRelationIsa.progressBplRelTac(MLUtil.contextAniquotation)),
    )
  }

  private def initBoogieStateProof(ctxtBplWfThm: String, outputStateRel: String): Seq[String] = {
    Seq(
      applyTac(unfoldTac(Seq(IsaUtil.definitionLemmaFromName(ViperBoogieRelationIsa.stateRelEmptyName),
                             IsaUtil.definitionLemmaFromName(boogieProg.procBodyAstDef)))),
      applyTac(BoogieIsaTerm.simplifyAstToProgramPointTac),
      applyTac(ViperBoogieRelationIsa.stmtRelPropagatePreTac),
      applyTac(BoogieIsaTerm.redAstBplRelTransitiveTac),
      applyTac(BoogieIsaTerm.redAstBplRelOutputStatelRelInstantiationTac(SecondConjunctStateRelInst)),
      applyTac(reflTac),
      applyTac(reflTac),
      applyTac(BoogieIsaTerm.unfoldASTBlockInGoalTac),
      applyTac(BoogieIsaTerm.redAstBplRelOneSimpleCmdTac),
      applyTac(ruleTac("exI")),
      applyTac(ruleTac("conjI")),
      applyTac(BoogieIsaTerm.assignIntroAltTac),
      applyTac(simpTac(boogieProg.getGlobalLookupTyThm(MaskGlobalVar))),
      applyTac(ruleTac(BoogieIsaTerm.redVarThm)),
      applyTac(ViperBoogieRelationIsa.zeroMaskLookupTactic(IsaUtil.definitionLemmaFromName(translationRecordName))),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName))),
    ) ++
    initUpdatedStateInRelation() ++
    initAssumeGoodState(ctxtBplWfThm, outputStateRel)
  }

  private def initUpdatedStateInRelation() : Seq[String] = {
    Seq(
      applyTac(simpTacOnly(ViperBoogieRelationIsa.boogieConstValSimpsThm)),
      applyTac(ruleTac(ViperBoogieRelationIsa.stateRelMaskUpdateThm)),
      applyTac(fastforceTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecordName))),
      applyTac(fastforceTacWithIntros(Seq(ViperBoogieRelationIsa.zeroMaskRelThm))),
      applyTac(simpTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecordName))),
      applyTac(simpTac),
      applyTac(simpTac)
    )
  }

  private def initAssumeGoodState(ctxtBplWfThm: String, outputStateRel: String) : Seq[String] = {
    Seq(
      applyTac(BoogieIsaTerm.redAstBplRelOutputStatelRelInstantiationTac(IdentityStateRelInst)),
      applyTac(BoogieIsaTerm.redAstBplRelOneSimpleCmdTac),
      applyTac(ruleTac("exI")),
      applyTac(ruleTac("conjI")),
      applyTac(ViperBoogieRelationIsa.redAssumeGoodStateTac(IsaUtil.definitionLemmaFromName(translationRecordName),
        MLUtil.isaToMLThm(ctxtBplWfThm)
        )),
      applyTac(assumeTac)
    )
  }

  private def not_satisfies_prop_set(list_all_ran_lemma: String) : String =
    ProofUtil.OF("not_satisfies_prop_in_set", list_all_ran_lemma)
}
