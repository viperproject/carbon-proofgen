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
import viper.carbon.proofgen.ViperIsaTerm.localVarAssign
import viper.carbon.proofgen.hints.UpdateStateComponentHint
import viper.silver.verifier.ModelParser.definition


case class MethodRelationalProofGenerator(
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

  val varRelationListName = "var_relation_list_1"
  val varRelationBoundedByName = "var_relation_list_1_bound"
  val translationRecord0Name = "tr_vpr_bpl_0"

  // A translation record that includes labeled boogie variables in the label_hm_translation
  val translationRecord1Name = "tr_vpr_bpl_1"
  val stateRelInitialName = "state_rel_initial"

  val stateRelWithHMTranslationName = "state_rel_tr_vpr_bpl_1"

  val funReprConcrete = TermIdent(ViperBoogieRelationIsa.funReprConcreteName)

  val typeInterpBplName = "type_interp_bpl"

  val basicDisjointnessLemmasName = "basic_disjointness_lemmas"

  // Pull out the Boogie variables corresponding to local heap and mask
  val (localHeapVar, localMaskVar) : (LocalVar, LocalVar) =
    methodProofHint.setupOldStateHint match {
      case Seq(
        UpdateStateComponentHint(HeapStateComponent, oldHeap, _),
        UpdateStateComponentHint(PermissionStateComponent, oldMask, _),
      ) => (
        oldHeap(0).asInstanceOf[LocalVar],
        oldMask(0).asInstanceOf[LocalVar]
      )
      case Seq(
        UpdateStateComponentHint(PermissionStateComponent, oldMask, _),
        UpdateStateComponentHint(HeapStateComponent, oldHeap, _),
      ) => (
        oldHeap(0).asInstanceOf[LocalVar],
        oldMask(0).asInstanceOf[LocalVar]
      )
      case _ => sys.error("Could not find old heap and old mask setup hints")
    }

  def generateRelationalProof() : (Theory, DefaultRelationalProofData) = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    val viperVarContextDef = DefDecl(
      varContextViperName,
      Some(ArrowType(IsaTypeUtil.natType, IsaTypeUtil.optionType(ViperIsaType.viperTyType))),
      (Seq(), IsaTermUtil.nthOption(IsaTermUtil.appendList(
        /* we use the types directly here instead of the method declaration, since then the proof of the relational
        lemma does not need to unfold the method declaration every time the Viper variable context is required
         */
        TermList(methodAccessor.origMethod.formalArgs.map(d => ViperIsaType.translate(d.typ))),
        TermList(methodAccessor.origMethod.formalReturns.map(d => ViperIsaType.translate(d.typ))),
      )))
    )

    outerDecls += viperVarContextDef

    val varRelationList = {
      //we need to sort the values such that we can prove injectivity of the mapping via an ordering lemma
      TermList(
        vprTranslation.availableVariables().map(vprVar => (vprVar, boogieProg.getBoogieVarId(vprVar))).sortBy(a => a._2)
          .map( {
           case (vprVar, bplVarId) =>
             val vprVarId = vprTranslation.translateVariableId(vprVar).get
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
        ViperBoogieIsaUtil.minInRangeOfList(varRelationList),
        ViperBoogieIsaUtil.maxInRangeOfList(varRelationList)
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(varRelationListName)))))
    )

    outerDecls += varRelationBoundedBy

    val translationRecord0 = TranslationRecord.makeTranslationRecord(
      heapVar = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVar = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      heapVarDef = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVarDef = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      fieldTranslation = IsaTermUtil.mapOf(progAccessor.fieldRel),
      funTranslation = TermIdent("f_None"),
      varTranslation = IsaTermUtil.mapOf(TermIdent(varRelationListDef.name)),
      /* TODO: the constants representation should be generated by Carbon and not hardcoded */
      constRepr = ViperBoogieRelationIsa.constReprBasic,
      labelHMTranslation = TermTuple(Seq(
        TermQuantifier(Lambda, Seq(Wildcard), TermIdent(SimpleIdentifier("None"))),
        TermQuantifier(Lambda, Seq(Wildcard), TermIdent(SimpleIdentifier("None")))
      )),
      stateRelOptions = TermIdent("default_state_rel_options")
    )

    val translationRecord1 = TranslationRecord.makeTranslationRecord(
      heapVar = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVar = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      heapVarDef = NatConst(globalBplData.getVarId(HeapGlobalVar)),
      maskVarDef = NatConst(globalBplData.getVarId(MaskGlobalVar)),
      fieldTranslation = IsaTermUtil.mapOf(progAccessor.fieldRel),
      funTranslation = TermIdent("f_None"),
      varTranslation = IsaTermUtil.mapOf(TermIdent(varRelationListDef.name)),
      /* TODO: the constants representation should be generated by Carbon and not hardcoded */
      constRepr = ViperBoogieRelationIsa.constReprBasic,
      // TODO correct term here
      labelHMTranslation = TermTuple(Seq(
        TermMap(Seq(
          (
            TermIdent(SimpleIdentifier("old_label")),
            NatConst(boogieProg.getVarId(localHeapVar))
          )
        )),
        TermMap(Seq(
          (
            TermIdent(SimpleIdentifier("old_label")),
            NatConst(boogieProg.getVarId(localMaskVar))
          )
        ))
      )),
      stateRelOptions = TermIdent("default_state_rel_options")
    )

    val translationRecord0Def = DefDecl(translationRecord0Name,
      Some(TranslationRecord.translationRecordType),
      (Seq(), translationRecord0))

    val translationRecord1Def = DefDecl(translationRecord1Name,
      Some(TranslationRecord.translationRecordType),
      (Seq(), translationRecord1))

    outerDecls += translationRecord0Def
    outerDecls += translationRecord1Def

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
            TermIdent(translationRecord0Def.name),
            IsaTermUtil.emptyMap,
            TermIdent("ctxt"),
            TermIdent("w"),
            TermIdent("ns")
          )
        )
      )

    outerDecls += stateRelInitialAbbrev

    val stateRelWithHMTranslation: AbbrevDecl =
      AbbrevDecl(
        stateRelWithHMTranslationName,
        None,
        (Seq(TermIdent("A"), TermIdent("Pr"), TermIdent("ctxt"), TermIdent("w"), TermIdent("ns")),
          ViperBoogieRelationIsa.stateRelationDefSame(
            TermIdent("Pr"),
            ViperBoogieRelationIsa.trivialStateConsistency,
            TypeRepresentation.makeBasicTypeRepresentation(TermIdent("A")),
            TermIdent(translationRecord1Def.name),
            IsaTermUtil.emptyMap,
            TermIdent("ctxt"),
            TermIdent("w"),
            TermIdent("ns")
          )
        )
      )
    
    outerDecls += stateRelWithHMTranslation

    outerDecls += BoogieIsaTerm.typeInterpBplAbbrev(typeInterpBplName)

    val basicDisjointnessLemmas = LemmasDecl(
      basicDisjointnessLemmasName,
      Seq(
        //if the list is empty, then boundedness lemma does not help, instead expose the full definition
        if(methodAccessor.origMethod.formalArgs.isEmpty && methodAccessor.origMethod.formalReturns.isEmpty) {
          IsaUtil.definitionLemmaFromName(varRelationListName)
        } else {
          not_satisfies_prop_set(ProofUtil.OF("list_all_ran_map_of", varRelationBoundedByName))
        },
        //if the list is empty, then boundedness lemma does not help, instead expose the full definition
        if(progAccessor.origProgram.fields.isEmpty) {
          IsaUtil.definitionLemmaFromName(progAccessor.fieldRel.id.toString)
        } else {
          not_satisfies_prop_set(ProofUtil.OF("list_all_ran_map_of", progAccessor.fieldRelBoundedLemma))
        },
        //const repr is not empty
        not_satisfies_prop_set("const_repr_basic_bound_2")
      )
    )

    outerDecls += basicDisjointnessLemmas

    val (relationalProofLocaleDecl, relationalProofLemmaName) = generateRelationalProofLocale()
    outerDecls += relationalProofLocaleDecl

    val theory =
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

    val relationalProofData = DefaultRelationalProofData(
      theoryName = theoryName,
      relationalLemmaData = RelationalLemmaData(IsaUtil.qualifyName(relationalProofLocaleDecl.name, relationalProofLemmaName), relationalProofLocaleDecl.name),
      translationRecord0DefName = translationRecord0Name,
      translationRecord1DefName = translationRecord1Name,
      varRelationListDefName = varRelationListName,
      varRelationBoundsLemmaName = varRelationBoundedBy.name,
      basicDisjointnessLemmaName = basicDisjointnessLemmasName,
      varContextVprDefName = viperVarContextDef.name,
      varContextBplDefName = varContextBoogieName
    )

    (theory, relationalProofData)
  }

  private def generateRelationalProofLocale() : (LocaleDecl, String) = {
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
      Seq( ( exprContextBpl, ViperBoogieRelationIsa.expressionContextType(VarType("a")) ),
           ( totalContextVpr, ViperIsaType.totalContext(VarType("a")))
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
        (Some(rtypeInterpEmpty + "[simp]"), TermBinary.eq(BoogieExpressionContext.rtypeInterp(exprContextBpl), TermList(Seq())))
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

    val stmtRelHints = "stmt_body_hints"
    val stmtInhalePreconditionHints = "stmt_precondition_hints"
    val stmtPostconditionFramingHints = "stmt_postcondition_framing_hints"
    val stmtExhalePostconditionHints = "stmt_postcondition_hints"

    val stmtPreconditionHintValue =
      if(methodAccessor.origMethod.pres.isEmpty) {
        None
      } else {
        Some(AtomicHint(InhaleStmtHint(Seq(InhaleStmtComponentHint(methodProofHint.preconditionInhaleHint)))))
      }

    val stmtPostconditionFramingValue =
      if(methodAccessor.origMethod.posts.isEmpty) {
        None
      } else {
        Some(AtomicHint(InhaleStmtHint(Seq(InhaleStmtComponentHint(methodProofHint.postconditionFramingHint._2)))))
      }

    val stmtPostconditionHintValue =
      if(methodAccessor.origMethod.posts.isEmpty) {
        None
      } else {
        Some(AtomicHint(ExhaleStmtHint(Seq(ExhaleStmtComponentHint(methodProofHint.postconditionExhaleHint.conjoinBodyHints)))))
      }


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
        MLUtil.defineVal(lookupVarRelTac,
          MLUtil.lambda(Seq("ctxt"),
            MLUtil.seqAllNewTac(
              MLUtil.simpAsm(isaToMLThms(Seq(definitionLemmaFromName(translationRecord0Name), definitionLemmaFromName(translationRecord1Name), definitionLemmaFromName(varRelationListName), definitionLemmaFromName(DeBruijnIsaUtil.shiftAndAddId))), "ctxt"),
              MLUtil.fastforceTac("[]", "ctxt")
            ))
        ),
        MLUtil.defineVal(simpWithTrDef, MLUtil.simpAsmSolved(isaToMLThms(Seq(definitionLemmaFromName(translationRecord0Name), definitionLemmaFromName(translationRecord1Name))))),
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
          methodAccessor.origProgram.fields.map(field => boogieProg.getGlobalLookupTyThm(FieldConst(field))) ++
          boogieProg.globalDataAccessor.predefinedConstants.map(const => boogieProg.getGlobalLookupTyThm(const))
        )),

        MLUtil.defineVal(lookupFunBplThms, isaToMLThms(
          functionProofGenInterface.allFunBplLookupLemmasForViperExpressions(methodAccessor.origProgram.functions)
        )),

        MLUtil.defineFun(heapReadWfTac, Seq("ctxt"),
          MLUtil.seqPrimeTac(
            MLUtil.app(MLUtil.simpAsm(MLUtil.isaToMLThms(Seq(definitionLemmaFromName(translationRecord0Name), definitionLemmaFromName(translationRecord1Name)))), "ctxt"),
            MLUtil.resolveTac("ctxt", MLUtil.isaToMLThms(Seq("heap_wf_concrete[OF CtxtWf wf_ty_repr_basic]")))
          )
        ),

        MLUtil.defineFun(heapReadMatchTac, Seq("ctxt"),
            MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
              definitionLemmaFromName(translationRecord0Name),
              definitionLemmaFromName(translationRecord1Name),
              definitionLemmaFromName(TypeRepresentation.tyReprBasicName),
              definitionLemmaFromName("read_heap_concrete"))
            ), "ctxt"),
          ),

        MLUtil.defineFun(fieldRelTac, Seq("ctxt"),
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(
            definitionLemmaFromName(translationRecord0Name),
            definitionLemmaFromName(translationRecord1Name),
            //definitionLemmaFromName(progAccessor.fieldRel.toString)
            progAccessor.allFieldLookupLemmas.fieldRelMapOfLemmas
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

        MLUtil.defineVal(auxVarDisjTac,
          //map_upd_set_dom for the method call case, shift_and_add is required when scoped variables are introduced
          MLUtil.simpAsmSolved(MLUtil.isaToMLThms(Seq(definitionLemmaFromName(translationRecord0Name), definitionLemmaFromName(translationRecord1Name), basicDisjointnessLemmasName,
            "map_upd_set_dom", "aux_pred_capture_state_dom", DeBruijnIsaUtil.ranShiftAndAddLemma, "vars_label_hm_tr_def", "active_labels_hm_tr_def")))
        ),

        MLUtil.defineVal(ProofGenMLConstants.basicStmtRelInfo, ViperBoogieMLUtil.createBasicStmtRelInfo(
          ctxtWfThm = isaToMLThm(bplCtxtWfLabel),
          vprProgramContextEqThm = isaToMLThm(vprProgramTotal),
          trDefThms = isaToMLThms(Seq(definitionLemmaFromName(translationRecord0Name), definitionLemmaFromName(translationRecord1Name))),
          methodDataTableMLIdent = progAccessor.methodDataTableML,
          varRelTac = lookupVarRelTac,
          varContextVprTac = "assm_full_simp_solved_with_thms_tac " +
            isaToMLThms(Seq(definitionLemmaFromName(varContextViperName), IsaUtil.definitionLemmaFromName(DeBruijnIsaUtil.shiftAndAddId))),
          fieldRelSingleTac = fieldRelSingleTac,
          auxVarDisjTac = auxVarDisjTac,
          tyInterpEContextBplEq = MLUtil.isaToMLThm(tyInterpEqBpl)
          )
        ),

        MLUtil.defineVal(ProofGenMLConstants.expRelInfo, ViperBoogieMLUtil.createExpRelInfo(
          ProofGenMLConstants.basicStmtRelInfo,
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
            expRelInfo = ProofGenMLConstants.expRelInfo
          )
        ),

        MLUtil.defineVal(ProofGenMLConstants.expWfRelInfo,
          ViperBoogieMLUtil.createExpWfRelInfo(
            ProofGenMLConstants.basicStmtRelInfo,
            fieldAccessWfRelTacAuxInst
          )
        ),

        MLUtil.defineVal(
          ProofGenMLConstants.inhaleRelInfo,
          ViperBoogieMLUtil.createInhaleRelInfo(
            basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
            atomicInhaleRelTac = "atomic_inhale_rel_inst_tac",
            isInhRelInvThm = MLUtil.isaToMLThm(InhaleRelUtil.isInhRelInvThm(true)),
            noDefChecksTacOpt = InhaleRelUtil.inhNoDefChecksTacOpt(true),
          )
        ),

        MLUtil.defineVal(
          ProofGenMLConstants.inhaleRelInfoWithoutDefChecks,
          ViperBoogieMLUtil.createInhaleRelInfo(
            basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
            atomicInhaleRelTac = "atomic_inhale_rel_inst_tac",
            isInhRelInvThm = MLUtil.isaToMLThm(InhaleRelUtil.isInhRelInvThm(false)),
            noDefChecksTacOpt = InhaleRelUtil.inhNoDefChecksTacOpt(false),
          )
        ),

        MLUtil.defineVal(
          ProofGenMLConstants.exhaleRelInfo,
          ViperBoogieMLUtil.createExhaleRelInfo(
            basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
            atomicExhaleRelTac = "atomic_exhale_rel_inst_tac",
            isExhRelInvThm = MLUtil.isaToMLThm(ExhaleRelUtil.isExhRelInvThm(true)),
            noDefChecksTacOpt = ExhaleRelUtil.exhNoDefChecksTacOpt(true)
          )
        ),

        MLUtil.defineVal(
          ProofGenMLConstants.exhaleRelInfoWithoutDefChecks,
          ViperBoogieMLUtil.createExhaleRelInfo(
            basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
            atomicExhaleRelTac = "atomic_exhale_rel_inst_tac",
            isExhRelInvThm = MLUtil.isaToMLThm(ExhaleRelUtil.isExhRelInvThm(false)),
            noDefChecksTacOpt = ExhaleRelUtil.exhNoDefChecksTacOpt(false)
          )
        ),

        MLUtil.defineVal(ProofGenMLConstants.stmtRelInfo, ViperBoogieMLUtil.createStmtRelInfo(
          basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
          atomicRelTac = "atomic_rel_inst_tac",
          inhaleRelInfo = ProofGenMLConstants.inhaleRelInfo,
          exhaleRelInfo = ProofGenMLConstants.exhaleRelInfo
        )),

        MLUtil.defineVal(ProofGenMLConstants.stmtRelInfoWithoutDefChecks, ViperBoogieMLUtil.createStmtRelInfo(
          basicStmtRelInfo = ProofGenMLConstants.basicStmtRelInfo,
          atomicRelTac = "atomic_rel_inst_tac",
          inhaleRelInfo = ProofGenMLConstants.inhaleRelInfoWithoutDefChecks,
          exhaleRelInfo = ProofGenMLConstants.exhaleRelInfoWithoutDefChecks
        )),

        MLUtil.defineVal(stmtRelHints, MLHintGenerator.generateStmtHintsInML(methodProofHint.bodyHint, boogieProg, ProofGenMLConstants.expWfRelInfo, ProofGenMLConstants.expRelInfo)),
      ) ++
      stmtPreconditionHintValue.fold[Seq[String]](Seq())(h => Seq(
          MLUtil.defineVal(stmtInhalePreconditionHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, ProofGenMLConstants.expWfRelInfo, ProofGenMLConstants.expRelInfo))
      )) ++
      stmtPostconditionFramingValue.fold[Seq[String]](Seq())(h => Seq(
        MLUtil.defineVal(stmtPostconditionFramingHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, ProofGenMLConstants.expWfRelInfo, ProofGenMLConstants.expRelInfo))
      )) ++
      stmtPostconditionHintValue.fold[Seq[String]](Seq())(h =>
        Seq (
          MLUtil.defineVal(stmtExhalePostconditionHints, MLHintGenerator.generateStmtHintsInML(h, boogieProg, ProofGenMLConstants.expWfRelInfo, ProofGenMLConstants.expRelInfo)),
        )
      )

    outerDecls += MLDecl(mlInitializationCode, MLNormal)

    val outputStateRel = TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, progAccessor.vprProgram, exprContextBpl))

    val stateRelOld = TermApp(TermIdent(stateRelWithHMTranslationName), Seq(absvalInterpVpr, progAccessor.vprProgram, exprContextBpl))

    val mainTheorem = LemmaDecl("method_rel_proof",
      ContextElem.empty(),
      ViperBoogieRelationIsa.methodRel(
        stateRelEnter=ViperBoogieRelationIsa.stateRelEmpty(TermApp(TermIdent(stateRelInitialName), Seq(absvalInterpVpr, progAccessor.vprProgram, exprContextBpl))),
        stateRelExit=outputStateRel,
        stateRelOld=stateRelOld,
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
        inhalePreconditionProof(ProofGenMLConstants.stmtRelInfo, stmtInhalePreconditionHints) ++
        oldHeapOldMaskSetupProof() ++
        postconditionFramingProof(methodProofHint.postconditionFramingHint._1, ProofGenMLConstants.basicStmtRelInfo, ProofGenMLConstants.stmtRelInfo, stmtPostconditionFramingHints) ++
        methodBodyAndPostconditionProof(ProofGenMLConstants.stmtRelInfo, stmtRelHints, stmtExhalePostconditionHints) ++
        Seq(doneTac)
      )
    )

    outerDecls += mainTheorem

    (LocaleDecl("method_proof", contextElem, outerDecls.toSeq), mainTheorem.name)
  }

  private def inhalePreconditionProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    applyTac(unfoldTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPrecondition))) +:
    (
      if(methodAccessor.origMethod.pres.isEmpty) {
        Seq(
          applyTac(ruleTac("inhale_stmt_rel_no_inv")), //TODO: do not hardcode theorem names
          applyTac(simp),
          applyTac(simp),
          applyTac(ruleTac("inhale_rel_true"))
        )
      } else {
        Seq(
          applyTac(ViperBoogieRelationIsa.stmtRelPropagatePostSameRelTac),
          applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
          applyTac(ViperBoogieRelationIsa.progressRedBplRelTac(MLUtil.contextAniquotation)),
        )
      }
    )
  }

  private def oldHeapOldMaskSetupProof() : Seq[String] = {
    Seq(
      applyTac(introTac("exI")),
      applyTac(introTac("conjI")),
      applyTac(ruleTac(where(
        "rel_propagate_post",
        "R1.0",
        TermQuantifier(
          Lambda,
          Seq(
            SimpleIdentifier("\\<omega>"),
            SimpleIdentifier("ns")
          ),
          TermBinary.and(
            TermApp(
              TermIdent(SimpleIdentifier("state_rel_well_def_same")),
              Seq(
                TermIdent(SimpleIdentifier("ectxt")),
                TermIdent(SimpleIdentifier("vpr_prog")),
                TermQuantifier(
                  Lambda,
                  Seq(Wildcard),
                  BoolConst(true)
                ),
                TermApp(
                  TermIdent(SimpleIdentifier("ty_repr_basic")),
                  TermApp(
                    TermIdent(SimpleIdentifier("absval_interp_total")),
                    TermIdent(SimpleIdentifier("ctxt_vpr"))
                  )
                ),
                TermIdent(translationRecord0Name),
                TermIdent(SimpleIdentifier("Map.empty")),
                TermIdent(SimpleIdentifier("\\<omega>")),
                TermIdent(SimpleIdentifier("ns"))
              )
            ),
            TermBinary.eq(
              TermApp(
                TermIdent(SimpleIdentifier("get_trace_total")),
                TermIdent(SimpleIdentifier("\\<omega>"))
              ),
              TermMap(Seq(
                (TermIdent(SimpleIdentifier("old_label")), TermApp(TermIdent(SimpleIdentifier("get_total_full")), TermIdent(SimpleIdentifier("\\<omega>"))))
              ))
            )
          )
        )
      ))),
      applyTac(ruleTac("rel_general_success_refl_2")),
      applyTac(simpTac),
      applyTac(ruleTac("state_rel_can_add_trace")),
      applyTac(simpTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(blastTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(ruleTac(where(
        "red_ast_bpl_rel_transitive_4",
        "Q",
        TermQuantifier(
          Lambda,
          Seq(
            SimpleIdentifier("\\<omega>")
          ),
          TermBinary.eq(
            TermApp(
              TermIdent(SimpleIdentifier("get_trace_total")),
              TermIdent(SimpleIdentifier("\\<omega>"))
            ),
            TermMap(Seq(
              (TermIdent(SimpleIdentifier("old_label")), TermApp(TermIdent(SimpleIdentifier("get_total_full")), TermIdent(SimpleIdentifier("\\<omega>"))))
            ))
          )
        )
      ))),
      applyTac(simpTac),
      applyTac(ruleTac("red_ast_bpl_relI")),
      applyTac(ruleTac("setup_oldm")),
      applyTac(blastTac),
      applyTac(simpTac),
      applyTac(simpTac),
      applyTac(MLUtil.mlTacticToIsa(
        MLUtil.app("aux_var_disj_tac", Seq(MLUtil.contextAniquotation, "1"))
      )),
      applyTac(fastforceTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(simpTac(Seq("ty_repr_basic_def", boogieProg.getLocalLookupTyThm(localMaskVar)))),
      applyTac(simpTac),
      applyTac(simpTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(ruleTac("red_ast_bpl_relI")),
      applyTac(ruleTac("setup_oldh")),
      applyTac(blastTac),
      applyTac(simpTac),
      applyTac(simpTac),
      applyTac(MLUtil.mlTacticToIsa(
        MLUtil.app("aux_var_disj_tac", Seq(MLUtil.contextAniquotation, "1"))
      )),
      applyTac(fastforceTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(simpTac(Seq("ty_repr_basic_def", boogieProg.getLocalLookupTyThm(localHeapVar)))),
      applyTac(simpTac),
      applyTac(simpTac(Seq(IsaUtil.definitionLemmaFromName(translationRecord0Name), IsaUtil.definitionLemmaFromName(translationRecord1Name)))),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name)))
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
      Seq(
        applyTac(ruleTac(ViperBoogieRelationIsa.postFramingRelAuxTrivialThm)),
        applyTac(simpTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPostcondition)))
      )
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

  private def methodBodyAndPostconditionProof(stmtRelInfo: String, stmtRelBodyTacHints: String, stmtRelPostTacHints: String) : Seq[String]  = {
      if(methodAccessor.origMethod.body.isEmpty) {
        //no proof obligations on the body and need not check postcondition
        Seq(
          applyTac(ruleTac("impI")),
          applyTac(simpTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody))),
        )
      } else {
        concreteMethodBodyProof(ProofGenMLConstants.stmtRelInfo, stmtRelBodyTacHints) ++
          exhalePostconditionProof(ProofGenMLConstants.stmtRelInfoWithoutDefChecks, stmtRelPostTacHints)
      }
  }

  //this method handles the method body for concrete methods (i.e., methods with a body)
  private def concreteMethodBodyProof(stmtRelInfo: String, stmtRelTacHints: String) : Seq[String] = {
    Seq(
      applyTac("(rule impI)+"),
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
        applyTac(ViperBoogieRelationIsa.progressRedBplRelTac(MLUtil.contextAniquotation)) //will progress the empty block
      )
    } else {
      Nil
    }) ++ Seq(
      applyTac(ViperBoogieRelationIsa.stmtRelTac(MLUtil.contextAniquotation, stmtRelInfo, stmtRelTacHints)),
      applyTac(ViperBoogieRelationIsa.progressRedBplRelTac(MLUtil.contextAniquotation)),
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
      applyTac(ViperBoogieRelationIsa.zeroMaskLookupTactic(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
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
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
      applyTac(fastforceTacWithIntros(Seq(ViperBoogieRelationIsa.zeroMaskRelThm))),
      applyTac(simpTac),
      applyTac(simpTac(IsaUtil.definitionLemmaFromName(translationRecord0Name))),
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
      applyTac(ViperBoogieRelationIsa.redAssumeGoodStateTac(
        Seq(
          IsaUtil.definitionLemmaFromName(translationRecord0Name)
        ),
        MLUtil.isaToMLThm(ctxtBplWfThm)
        )),
      applyTac(assumeTac)
    )
  }

  private def not_satisfies_prop_set(list_all_ran_lemma: String) : String =
    ProofUtil.OF("not_satisfies_prop_in_set", list_all_ran_lemma)
}
