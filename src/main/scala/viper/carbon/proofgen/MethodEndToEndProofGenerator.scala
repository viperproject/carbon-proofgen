package viper.carbon.proofgen

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

  def generatePartialEndToEndProof(): Theory = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    outerDecls += DeclareDecl("Nat.One_nat_def[simp del]")
    outerDecls += DeclareDecl(s"${BoogieExpressionContext.exprContextRecordName}.defs(1)[simp]")

    val typeInterpBplAbbrev = BoogieIsaTerm.typeInterpBplAbbrev("type_interp_bpl")
    outerDecls += typeInterpBplAbbrev

    val ctxtBplDef = DefDecl("ctxt_bpl", None,
      (Seq(), BoogieExpressionContext.makeRecord(
        typeInterp = TermApp(TermIdent(typeInterpBplAbbrev.name), ViperTotalContext.absvalInterpTotal(endToEndData.ctxtVpr)),
        varContext = relationalProofData.varContextBplDef,
        funInterp = endToEndData.funInterpVprBpl,
        rtypeInterp = TermList(Seq())
      ))
    )

    val ctxtBplTerm = TermIdent(ctxtBplDef.name)

    outerDecls += ctxtBplDef

    val varContextBplEqLemma = LemmaDecl("var_context_bpl_eq",
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
    val initTacCommands = Seq(
        ProofUtil.ruleTac(ProofUtil.OF("end_to_end_vpr_method_correct_partial", Seq("assms", ViperBoogieRelationIsa.trueMonoPropDownwardOrdLemmaName))),
        ProofUtil.simpTacOnly(endToEndData.programTotalProgEqLemma),
        ProofUtil.ruleTac(ProofUtil.simplified(viperProgAccessor.allMethodsAccessor.lookupLemmaName(methodAccessor.origMethod.name),
          ProofUtil.OF("HOL.sym", viperProgAccessor.methodsProgEqLemma))),
        ProofUtil.ruleTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody)),
        ProofUtil.simpTac(Seq(IsaMethodPrecondition, IsaMethodPostcondition).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        ProofUtil.simpTac(Seq(IsaMethodPrecondition, IsaMethodArgTypes).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        ProofUtil.simpTac,
        ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(boogieProgAccessor.procBodyAstDef)),
        ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName(boogieProgAccessor.procBodyAstDef), IsaUtil.definitionLemmaFromName(boogieProgAccessor.preconditionDef.id.toString))),
        ProofUtil.ruleTac("HOL.refl"),
        ProofUtil.simpTacOnly(Seq(viperProgAccessor.methodsProgEqLemma, methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes), methodAccessor.methodDeclProjectionLemmaName(IsaMethodRetTypes)))
    )

    val initTac = initTacCommands.map(f => ProofUtil.applyTac(f))

    Proof(initTac)
  }

}
