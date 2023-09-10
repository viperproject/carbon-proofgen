package viper.carbon.proofgen

import isabelle.ast
import isabelle.ast.{BoolConst, ContextElem, DeclareDecl, DefDecl, IsaTermUtil, IsaThmUtil, IsaUtil, Lambda, LemmaDecl, LemmasDecl, OuterDecl, Proof, ProofUtil, TermApp, TermBinary, TermIdent, TermList, TermQuantifier, TermWithExplicitType, Theory, VarType}
import viper.carbon.proofgen.end_to_end.IsaViperEndToEndGlobalData
import viper.silver.{ast => sil}

import scala.collection.mutable.ListBuffer


case class MethodEndToEndProofData()

case class MethodEndToEndProofGenerator( theoryName: String,
                                         methodAccessor: IsaViperMethodAccessor,
                                         viperProgAccessor: IsaViperGlobalDataAccessor,
                                         endToEndData: IsaViperEndToEndGlobalData,
                                         relationalProofData: RelationalProofData,
                                         boogieProgAccessor: IsaBoogieProcAccessor) {

  private val translationRecordDefLemma = IsaUtil.definitionLemmaFromName(relationalProofData.translationRecordDef.id.toString)

  private val ctxtBplName = "ctxt_bpl"
  private val ctxtBplDefLemmaName = IsaUtil.definitionLemmaFromName("ctxt_bpl")

  private val varCtxtBplEqLemmaName = "var_context_bpl_eq"

  private val translationRecordDisjointLemmaName = "disjoint_property_aux"

  private val typeReprBasicDefLemmaName = IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName)

  def generatePartialEndToEndProof(): Theory = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    outerDecls += DeclareDecl("Nat.One_nat_def[simp del]")
    outerDecls += DeclareDecl(s"${BoogieExpressionContext.exprContextRecordName}.defs(1)[simp]")

    val typeInterpBplAbbrev = BoogieIsaTerm.typeInterpBplAbbrev("type_interp_bpl")
    outerDecls += typeInterpBplAbbrev

    val ctxtBplDef = DefDecl(ctxtBplName, None,
      (Seq(), BoogieExpressionContext.makeRecord(
        typeInterp = TermApp(TermIdent(typeInterpBplAbbrev.name), ViperTotalContext.absvalInterpTotal(endToEndData.ctxtVpr)),
        varContext = relationalProofData.varContextBplDef,
        funInterp = endToEndData.funInterpVprBpl,
        rtypeInterp = TermList(Seq())
      ))
    )

    val ctxtBplTerm = TermIdent(ctxtBplDef.name)

    outerDecls += ctxtBplDef

    val varContextBplEqLemma = LemmaDecl(varCtxtBplEqLemmaName,
      TermBinary.eq(BoogieExpressionContext.varContext(ctxtBplTerm), relationalProofData.varContextBplDef),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(ctxtBplDef.name)))))
    )

    outerDecls += varContextBplEqLemma

    val ctxtBplWfLemma = LemmaDecl("ctxt_wf",
      BoogieExpressionContext.wellFormed(
        viperProgram = viperProgAccessor.vprProgram,
        tyReprBpl = TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(endToEndData.ctxtVpr)),
        fieldMap = IsaTermUtil.mapOf(viperProgAccessor.fieldRel),
        funMap = TermIdent(ViperBoogieRelationIsa.funReprConcreteName),
        exprContext = ctxtBplTerm),
      Proof(
        Seq(
          ProofUtil.applyTac(ProofUtil.unfoldTac(IsaUtil.definitionLemmaFromName(BoogieExpressionContext.wfId.toString))),
          ProofUtil.byTac(ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName(ctxtBplDef.name), endToEndData.funInterpVprBplWfLemma)))
        )
      )
    )

    outerDecls += ctxtBplWfLemma

    val boundLemmas = LemmasDecl("bound_lemmas",
      Seq(
        ProofUtil.OF(IsaThmUtil.listAllRanMapOfLemma, relationalProofData.varRelationBoundsLemma),
        ProofUtil.OF(IsaThmUtil.listAllRanMapOfLemma, viperProgAccessor.fieldRelBoundedLemma),
        ViperBoogieIsaUtil.constReprBoundLemmaName
      )
    )

    outerDecls += boundLemmas

    val intersectionBoundLemmas = LemmasDecl("inter_bound_lemmas",
      Seq(
        ProofUtil.OF(IsaThmUtil.interDisjointIntervalsLemma, s"${boundLemmas.name}(1,2)"),
        ProofUtil.OF(IsaThmUtil.interDisjointIntervalsLemma, s"${boundLemmas.name}(1,3)"),
        ProofUtil.OF(IsaThmUtil.interDisjointIntervalsLemma, s"${boundLemmas.name}(2,3)")
      )
    )

    outerDecls += intersectionBoundLemmas

    val disjVarsStateRel = LemmaDecl("disjoint_property_aux",
      TermApp(TermIdent("disj_vars_state_relation"), Seq(relationalProofData.translationRecordDef, IsaTermUtil.emptyMap)),
      Proof(Seq(
        ProofUtil.applyTac(ProofUtil.ruleTac("disj_vars_state_relation_initialI")),
        ProofUtil.applyTac(ProofUtil.simpTac(translationRecordDefLemma)),
        ProofUtil.applyTac(ProofUtil.simpTac(translationRecordDefLemma)),
        ProofUtil.applyTac(ProofUtil.ruleTac("disj_helper_tr_vpr_bpl")),
        ProofUtil.byTac(ProofUtil.repeatTac(ProofUtil.forceTacWithSimps(
          Seq(translationRecordDefLemma, IsaUtil.definitionLemmaFromName(relationalProofData.varRelationListDef.id.toString),
            relationalProofData.basicDisjointnessLemma,
            intersectionBoundLemmas.name)))),
      ))
    )

    outerDecls += disjVarsStateRel

    outerDecls += generateMethodPartialLemma(typeInterpBplAbbrev.name)

    Theory(theoryName, Seq(relationalProofData.theoryName, "../"+endToEndData.theoryName), outerDecls.toSeq)
  }

  private def generateMethodPartialLemma(typeInterpBplName: String) : LemmaDecl = {

    LemmaDecl("method_partial_proof",
      ContextElem.onlyAssumptionsNoLabels(
        Seq(BoogieIsaTerm.procIsCorrect(
          typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(endToEndData.ctxtVpr)),
          functionDecls = boogieProgAccessor.globalDataAccessor.funDecls,
          constDecls = boogieProgAccessor.globalDataAccessor.constDecls,
          globalVarDecls = boogieProgAccessor.globalDataAccessor.globalDecls,
          axiomDecls = boogieProgAccessor.globalDataAccessor.axiomDecls,
          proc = boogieProgAccessor.procDef,
          vprDomainValueType = VarType("a")
        ))
      ),
      ViperMethodCorrectness.correctPartial(
        totalContext = TermWithExplicitType(endToEndData.ctxtVpr, ViperTotalContext.totalContextRecordType(VarType("a"))),
        stateConsistency = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), BoolConst(true)),
        methodAccessor.methodDecl
      ),
      generateMethodPartialProof()
    )
  }

  private def generateMethodPartialProof() : Proof  = {
    val methods =
      ProofUtil.applyTac(
        ProofUtil.ruleTac(s"end_to_end_vpr_method_correct_partial[" +
        s"where ?ctxt = $ctxtBplName, " +
        s"OF assms ${ViperBoogieRelationIsa.trueMonoPropDownwardOrdLemmaName} ${TypeRepresentation.wfTyReprBasicLemma} ${ViperBoogieRelationIsa.wfTotalConsistencyTrivialLemmaName}" +
        s"]"
        )
      ) +:
      (generateViperPropertiesProof().methods ++
       generateBoogiePropertiesProof().methods ++
        (generateMethodRelProof().methods :+ ProofUtil.applyTac(
          ProofUtil.simpTac(
            Seq(translationRecordDefLemma,
              IsaUtil.definitionLemmaFromName(ViperBoogieRelationIsa.defaultStateRelOptionsName)
            )
          )
        )) ++
       generateInitialStateRelProof().methods)

    Proof(methods)
  }

  private def generateViperPropertiesProof() : Proof = {
    val methodsWithoutApply =
      Seq(
        ProofUtil.simpTac(typeReprBasicDefLemmaName),
        ProofUtil.simpTacOnly(endToEndData.programTotalProgEqLemma),
        ProofUtil.ruleTac(ProofUtil.simplified(viperProgAccessor.allMethodsAccessor.lookupLemmaName(methodAccessor.origMethod.name),
        ProofUtil.OF("HOL.sym", viperProgAccessor.methodsProgEqLemma))),
        ProofUtil.ruleTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody)),
        ProofUtil.simpTac(Seq(IsaMethodPrecondition, IsaMethodPostcondition).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        ProofUtil.simpTac(Seq(IsaMethodPrecondition, IsaMethodArgTypes).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        ProofUtil.simpTac,
        ProofUtil.simpTacOnly(Seq(methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes), methodAccessor.methodDeclProjectionLemmaName(IsaMethodRetTypes)))
      )

    Proof(ProofUtil.mapApplyTac(methodsWithoutApply))
  }

  private def generateBoogiePropertiesProof() : Proof = {
    val procBplDefLemma = IsaUtil.definitionLemmaFromName(boogieProgAccessor.procDef.id.toString)

    val methodsWithoutApply =
      Seq(
        ProofUtil.simpTac(Seq(endToEndData.funInterpBplWfLemma, ctxtBplDefLemmaName)),
        ProofUtil.simpTac(ctxtBplDefLemmaName),
        ProofUtil.simpTac(ctxtBplDefLemmaName),
        ProofUtil.simpTac(Seq(procBplDefLemma, ctxtBplDefLemmaName)),
        ProofUtil.simpTac(Seq(procBplDefLemma, IsaUtil.definitionLemmaFromName(boogieProgAccessor.preconditionDef.id.toString))),
        ProofUtil.simpTac(procBplDefLemma),
        ProofUtil.simpTac(procBplDefLemma),
      )

    Proof(ProofUtil.mapApplyTac(methodsWithoutApply))
  }

  private def generateMethodRelProof() : Proof = {
    val methodsWithoutApply = Seq(
      //the method project lemmas are for the variable context
      ProofUtil.simpTacOnly(Seq(endToEndData.programTotalProgEqLemma, methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes), methodAccessor.methodDeclProjectionLemmaName(IsaMethodRetTypes))),
      ProofUtil.ruleTac(ProofUtil.simplified(relationalProofData.relationalLemmaName,
        IsaUtil.definitionLemmaFromName(relationalProofData.varContextVprDef.id.toString))
      ),
      ProofUtil.simpTacOnly(IsaUtil.definitionLemmaFromName(relationalProofData.relationalLemmaAssumptionDefName)),
      ProofUtil.introTac("conjI"),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.ruleTac(BoogieExpressionContext.wfId.id.toString),
      ProofUtil.simpTac(Seq(endToEndData.funInterpBplWfLemma, ctxtBplDefLemmaName)),
      ProofUtil.simpTac(endToEndData.programTotalProgEqLemma),
      ProofUtil.simpTac(ctxtBplDefLemmaName)
    )

    Proof(methodsWithoutApply.map(f => ProofUtil.applyTac(f)))
  }

  private def generateInitialStateRelProof() : Proof = {

    val initTac = Seq(
      ProofUtil.applyTac(ProofUtil.repeatTac(ProofUtil.ruleTac("exI"))),
      ProofUtil.applyTac(ProofUtil.introTac("conjI")),
      ProofUtil.preferTac(2)
    )

    val methodsWithoutApply =
      Seq(
      ProofUtil.ruleTac(ProofUtil.OF("init_state_in_state_relation", Seq(TypeRepresentation.wfTyReprBasicLemma, translationRecordDisjointLemmaName))),
      ProofUtil.simp, //empty state
      ProofUtil.simp, //total heap well typed
      ProofUtil.fastforceTacWithIntros(Seq(ViperStateIsaUtil.emptyStateHasValidMaskLemma)), //valid mask
      ProofUtil.simp, //state consistency
      ProofUtil.simpTac(ctxtBplDefLemmaName), //type interp equality
      ProofUtil.simpTac(typeReprBasicDefLemmaName), //domain type equality
      ProofUtil.simp, //Boogie state equality
    ) ++
      varTranslationInjectiveProof() ++
      Seq(
        ProofUtil.simpTac( // constant and global variable types closed
          Seq(ctxtBplDefLemmaName, boogieProgAccessor.constsWfThm, boogieProgAccessor.globalsWfThm, BoogieIsaTerm.closedWfTyFunEqThm)
        ),
        ProofUtil.simpTac( // parameter and local variable types closed
          Seq(ctxtBplDefLemmaName, boogieProgAccessor.paramsWfThm, boogieProgAccessor.localsWfThm, BoogieIsaTerm.closedWfTyFunEqThm)
        ),
      ) ++
      globalsLocalsDisjointProof() ++
      Seq(
        ProofUtil.simpTac(translationRecordDefLemma), // heap var and heap var def same
        ProofUtil.simpTac(translationRecordDefLemma) // mask var and mask var def same
      ) ++
      fieldRelInjectiveProof() ++
      Seq(
        ProofUtil.simpTac(Seq(translationRecordDefLemma, ViperBoogieRelationIsa.constReprBasicInjLemmaName)) //injectivity const representation
      ) ++
      fieldRelPropertyProof() ++
      Seq(
        //the declared types for the heap and mask are correct
        ProofUtil.simpTac(Seq(translationRecordDefLemma, ctxtBplDefLemmaName, typeReprBasicDefLemmaName,
            ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, boogieProgAccessor.globalDataAccessor.getGlobalMapOfThm(HeapGlobalVar))
          )
        ),
        ProofUtil.simpTac(Seq(translationRecordDefLemma, ctxtBplDefLemmaName, typeReprBasicDefLemmaName,
          ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, boogieProgAccessor.globalDataAccessor.getGlobalMapOfThm(MaskGlobalVar))
        ))
      ) ++
        Seq( ProofUtil.simpTac(Seq(translationRecordDefLemma, ctxtBplDefLemmaName, endToEndData.constantsLookupWithGlobalsLemma)) //constant declarations match the expected types
        ) ++
        varRelPropertyProof() ++
        (ProofUtil.blastTac +: //state equality ++
        axiomsProof())

    Proof(initTac ++ ProofUtil.mapApplyTac(methodsWithoutApply) :+ "done")
  }

  private def varTranslationInjectiveProof() : Seq[String] = {
    Seq(
      ProofUtil.simpTac(translationRecordDefLemma),
      ProofUtil.ruleTac(IsaThmUtil.strictlyOrderedListInjMapOfLemma),
      ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(relationalProofData.varRelationListDef.id.toString))
    )
  }

  private def globalsLocalsDisjointProof() : Seq[String] = {
    Seq(
      ProofUtil.cutTac(boogieProgAccessor.globalsLocalsDisjThm),
      ProofUtil.simpTac(ctxtBplDefLemmaName)
    )
  }

  private def fieldRelInjectiveProof() : Seq[String] = {
    Seq(
      ProofUtil.simpTac(translationRecordDefLemma),
      ProofUtil.ruleTac(IsaThmUtil.strictlyOrderedListInjMapOfLemma),
      ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(viperProgAccessor.fieldRel.id.toString))
    )
  }

  private def fieldRelPropertyProof() : Seq[String] = {
    Seq(
      ProofUtil.simpTac(
        Seq(IsaUtil.definitionLemmaFromName(viperProgAccessor.vprProgram.id.toString), translationRecordDefLemma, endToEndData.programTotalProgEqLemma)
      ),
      ProofUtil.simpTacOnly(Seq("fst_conv", varCtxtBplEqLemmaName)),
      ProofUtil.eruleTac(ProofUtil.OF(IsaThmUtil.listAll2MapOf, Seq(endToEndData.fieldPropLemma, "field_tr_prop_fst")))
    )
  }

  private def varRelPropertyProof() : Seq[String] = {
    Seq(
      ProofUtil.ruleTac(ProofUtil.where("var_rel_prop_aux", "var_rel_list", relationalProofData.varRelationListDef)),
      ProofUtil.simp,
      ProofUtil.simpTac(typeReprBasicDefLemmaName),
      ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(relationalProofData.varRelationListDef.id.toString) +: (Seq(IsaMethodArgTypes, IsaMethodRetTypes).map(methodAccessor.methodDeclProjectionLemmaName))),
      ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName("var_rel_prop"), ctxtBplDefLemmaName)),
      ProofUtil.simpTac( typeReprBasicDefLemmaName +: (methodAccessor.origMethod.formalArgs ++ methodAccessor.origMethod.formalReturns).map(
        decl => ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm,
          boogieProgAccessor.mapOfThmFromId(boogieProgAccessor.getBoogieVarId(decl.localVar)))
      )),
      ProofUtil.simpTac(translationRecordDefLemma)
    )
  }

  private def axiomsProof() : Seq[String] = {
    Seq(
      ProofUtil.ruleTac("boogie_axioms_state_restriction_aux"),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.simpTac(translationRecordDefLemma),
      ProofUtil.simpTac(Seq(translationRecordDefLemma, endToEndData.constantsLookupWithoutGlobalsLemma)),
      s"(disjoint_globals_aux_tac disj_prop_aux: ${ProofUtil.simplified("disjoint_property_aux", IsaUtil.definitionLemmaFromName("disj_vars_state_relation"))})",
      ProofUtil.simpTac(Seq(translationRecordDefLemma, ViperBoogieRelationIsa.constReprBasicInjLemmaName)),
      ProofUtil.simpTac(Seq(translationRecordDefLemma, IsaUtil.definitionLemmaFromName(boogieProgAccessor.globalDataAccessor.constDecls.id.toString))),
      ProofUtil.simpTac(Seq(ViperBoogieRelationIsa.constReprBasicRangeLemmaName, endToEndData.ranFieldRelLemma)),
      ProofUtil.fastforceTac,
      ProofUtil.cutTac(endToEndData.axiomSatLemma),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.simp
    )
  }

}
