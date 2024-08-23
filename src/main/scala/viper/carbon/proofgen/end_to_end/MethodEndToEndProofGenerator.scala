package viper.carbon.proofgen.end_to_end

import isabelle.ast._
import viper.carbon.proofgen._

import scala.collection.mutable.ListBuffer


trait MethodEndToEndProofData {

  def theoryName : String

  def endToEndLemma : String

  /**
    * The term provides the only assumption for [[endToEndLemma]].
    */
  def boogieProcCorrectAssumption : Term


}

case class DefaultMethodEndToEndProofData(override val theoryName : String,
                                          endToEndLemmaName: String,
                                          override val boogieProcCorrectAssumption: Term) extends MethodEndToEndProofData
{

  override def endToEndLemma: String = IsaUtil.qualifyName(theoryName, endToEndLemmaName)

}

case class MethodEndToEndProofGenerator( theoryName: String,
                                         methodAccessor: IsaViperMethodAccessor,
                                         viperProgAccessor: IsaViperGlobalDataAccessor,
                                         endToEndData: IsaViperEndToEndGlobalData,
                                         relationalProofData: RelationalProofData,
                                         boogieProgAccessor: IsaBoogieProcAccessor) {

  private val translationRecord0Name = relationalProofData.translationRecord0Def.id.toString
  private val translationRecord0DefLemma = IsaUtil.definitionLemmaFromName(translationRecord0Name)

  // The second translation record is necessary to capture the state after the old state has been set up
  private val translationRecord1Name = relationalProofData.translationRecord1Def.id.toString
  private val translationRecord1DefLemma = IsaUtil.definitionLemmaFromName(translationRecord1Name)

  private val ctxtBplName = "ctxt_bpl"
  private val ctxtBplDefLemmaName = IsaUtil.definitionLemmaFromName("ctxt_bpl")

  private val varCtxtBplEqLemmaName = "var_context_bpl_eq"

  private val translationRecordDisjointLemmaName = "disjoint_property_aux"

  private val typeReprBasicDefLemmaName = IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName)

  def generatePartialEndToEndProof(): (Theory, MethodEndToEndProofData) = {
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
      TermApp(TermIdent("disjoint_list"), TermApp(TermIdent("state_rel0_disj_list"), Seq(TermIdent(translationRecord0Name), TermIdent("Map.empty")))),
      Proof(Seq(
        ProofUtil.applyTac(ProofUtil.ruleTac("disj_vars_state_relation_initialI")),
        ProofUtil.applyTac(ProofUtil.simpTac(translationRecord0DefLemma)),
        ProofUtil.applyTac(ProofUtil.simpTac(translationRecord0DefLemma)),
        ProofUtil.applyTac(ProofUtil.simpTac(translationRecord0DefLemma)),
        ProofUtil.applyTac(ProofUtil.ruleTac("disj_helper_tr_vpr_bpl")),
        ProofUtil.byTac(ProofUtil.repeatTac(ProofUtil.forceTacWithSimps(
          Seq(translationRecord0DefLemma, IsaUtil.definitionLemmaFromName(relationalProofData.varRelationListDef.id.toString),
            relationalProofData.basicDisjointnessLemma,
            intersectionBoundLemmas.name)))),
      ))
    )

    outerDecls += disjVarsStateRel

    val (endToEndLemma, boogieProcCorrectAssm) = generateMethodPartialLemma(typeInterpBplAbbrev.name)
    outerDecls += endToEndLemma

    val methodEndToEndProofData = DefaultMethodEndToEndProofData(theoryName, endToEndLemma.name, boogieProcCorrectAssm)

    val theory = Theory(theoryName, Seq(relationalProofData.theoryName, "../"+endToEndData.theoryName), outerDecls.toSeq)

    (theory, methodEndToEndProofData)
  }

  private def generateMethodPartialLemma(typeInterpBplName: String) : (LemmaDecl, Term) = {

    val boogieProcCorrectAssm = BoogieIsaTerm.procIsCorrect(
          typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(endToEndData.ctxtVpr)),
          functionDecls = boogieProgAccessor.globalDataAccessor.funDecls,
          constDecls = boogieProgAccessor.globalDataAccessor.constDecls,
          uniqueConstNames = boogieProgAccessor.globalDataAccessor.uniqueConsts,
          globalVarDecls = boogieProgAccessor.globalDataAccessor.globalDecls,
          axiomDecls = boogieProgAccessor.globalDataAccessor.axiomDecls,
          proc = boogieProgAccessor.procDef,
          vprDomainValueType = endToEndData.abstractValueType
        )

    val lemma =
      LemmaDecl("method_partial_proof",
        ContextElem.onlyAssumptionsNoLabels(
          Seq(boogieProcCorrectAssm)
        ),
        ViperMethodCorrectness.correctPartial(
          totalContext = TermWithExplicitType(endToEndData.ctxtVpr, ViperTotalContext.totalContextRecordType(endToEndData.abstractValueType)),
          stateConsistency = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), BoolConst(true)),
          methodAccessor.methodDecl
        ),
        generateMethodPartialProof()
      )

    (lemma, boogieProcCorrectAssm)
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
            // TODO here
            Seq(translationRecord1DefLemma,
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
        methodAccessor.origMethod.body.fold(
          ProofUtil.simpTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody))
        )(_ => ProofUtil.ruleTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody))),
        // constraints specs
        ProofUtil.simpTac(Seq(IsaMethodPrecondition, IsaMethodPostcondition).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        // free variables in precondition must be argument variables
        ProofUtil.fastforceTacWithSimps(Seq(IsaMethodPrecondition, IsaMethodArgTypes).map(member => methodAccessor.methodDeclProjectionLemmaName(member))),
        //argument variables are not modified by the body
        methodAccessor.origMethod.body.fold(
          ProofUtil.simpTac(methodAccessor.methodDeclProjectionLemmaName(IsaMethodBody))
        )(_ => ProofUtil.fastforceTacWithSimps(Seq(IsaUtil.definitionLemmaFromName(DeBruijnIsaUtil.shiftDownSet), methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes)))),
        //variable context equality
        ProofUtil.simpTac(Seq(methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes), methodAccessor.methodDeclProjectionLemmaName(IsaMethodRetTypes)))
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
        //simplify vpr var context such that it matches
        ProofUtil.simplified(IsaUtil.definitionLemmaFromName(relationalProofData.varContextVprDef.id.toString), Seq()))
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
        ProofUtil.simpTac(translationRecord0DefLemma), // heap var and heap var def same
        ProofUtil.simpTac(translationRecord0DefLemma), // mask var and mask var def same
        ProofUtil.simpTac(translationRecord0DefLemma) // label_hm_translation empty
      ) ++
      fieldRelInjectiveProof() ++
      Seq(
        ProofUtil.simpTac(Seq(translationRecord0DefLemma, ViperBoogieRelationIsa.constReprBasicInjLemmaName)) //injectivity const representation
      ) ++
      fieldRelPropertyFullProof(true) ++
      Seq(
        //the declared types for the heap and mask are correct
        ProofUtil.simpTac(Seq(translationRecord0DefLemma, ctxtBplDefLemmaName, typeReprBasicDefLemmaName,
            ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, boogieProgAccessor.globalDataAccessor.getGlobalMapOfThm(HeapGlobalVar))
          )
        ),
        ProofUtil.simpTac(Seq(translationRecord0DefLemma, ctxtBplDefLemmaName, typeReprBasicDefLemmaName,
          ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, boogieProgAccessor.globalDataAccessor.getGlobalMapOfThm(MaskGlobalVar))
        ))
      ) ++
        Seq( ProofUtil.simpTac(Seq(translationRecord0DefLemma, ctxtBplDefLemmaName, endToEndData.constantsLookupWithGlobalsLemma)) //constant declarations match the expected types
        ) ++
        varRelPropertyProof() ++
        (ProofUtil.blastTac +: //state equality ++
          (uniqueConstantsProof() ++
          axiomsProof())
        )

    Proof(initTac ++ ProofUtil.mapApplyTac(methodsWithoutApply) :+ ProofUtil.doneTac)
  }

  private def varTranslationInjectiveProof() : Seq[String] = {
    Seq(
      ProofUtil.simpTac(translationRecord0DefLemma),
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
      ProofUtil.simpTac(translationRecord0DefLemma),
      ProofUtil.ruleTac(IsaThmUtil.strictlyOrderedListInjMapOfLemma),
      ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(viperProgAccessor.fieldRel.id.toString))
    )
  }

  private def fieldRelPropertyFullProof(withGlobals: Boolean) : Seq[String] = {
    val helperThm = if(withGlobals) { endToEndData.fieldPropWithGlobalsLemma } else { endToEndData.fieldPropWithoutGlobalsLemma }
    Seq(
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, varCtxtBplEqLemmaName, helperThm))
    )
  }

  private def varRelPropertyProof() : Seq[String] = {
    Seq(
      ProofUtil.ruleTac(ProofUtil.where("var_rel_prop_aux", "var_rel_list", relationalProofData.varRelationListDef)),
      ProofUtil.simp,
      ProofUtil.simpTac(typeReprBasicDefLemmaName),
      ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(relationalProofData.varRelationListDef.id.toString) +: (Seq(IsaMethodArgTypes, IsaMethodRetTypes).map(methodAccessor.methodDeclProjectionLemmaName)))
    ) ++
      (if(methodAccessor.origMethod.formalArgs.isEmpty && methodAccessor.origMethod.formalReturns.isEmpty) {
        Nil
      } else {
        Seq(
          ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName("var_rel_prop"), ctxtBplDefLemmaName)),
          ProofUtil.simpTac(typeReprBasicDefLemmaName +: (methodAccessor.origMethod.formalArgs ++ methodAccessor.origMethod.formalReturns).map(
            decl => ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm,
              boogieProgAccessor.mapOfThmFromId(boogieProgAccessor.getBoogieVarId(decl.localVar)))
          ))
        )
      }) :+
      ProofUtil.simpTac(translationRecord0DefLemma)
  }

  private def uniqueConstantsProof() : Seq[String] = {
    val uniqueConstsDefLemma = IsaUtil.definitionLemmaFromName(boogieProgAccessor.globalDataAccessor.uniqueConsts.toString)

    Seq(
      ProofUtil.ruleTac("unique_constants_initial_global_state"),
    ) ++
    disjointGlobalDeclarationsProof() ++
    Seq(
      ProofUtil.simpTac(uniqueConstsDefLemma),
      ProofUtil.ruleTac("unique_consts_field_prop"),
      ProofUtil.simpTac(Seq(uniqueConstsDefLemma, endToEndData.ranFieldRelLemma, translationRecord0DefLemma)),
      ProofUtil.simpTac(translationRecord0DefLemma),
      ProofUtil.ruleTac(endToEndData.declaredFieldsFieldRelDomEqLemma)
    )
  }

  private def axiomsProof() : Seq[String] = {
    Seq(
      ProofUtil.ruleTac("boogie_axioms_state_restriction_aux"),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.simpTac(translationRecord0DefLemma),
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, endToEndData.constantsLookupWithoutGlobalsLemma))
    ) ++
    fieldRelPropertyFullProof(false) ++
    disjointGlobalDeclarationsProof() ++
    Seq(
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, ViperBoogieRelationIsa.constReprBasicInjLemmaName)),
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, endToEndData.injFieldRelLemma)),
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, IsaUtil.definitionLemmaFromName(boogieProgAccessor.globalDataAccessor.constDecls.id.toString))),
      ProofUtil.simpTacOnly(Seq(ViperBoogieRelationIsa.constReprBasicRangeLemmaName, endToEndData.ranFieldRelLemma)),
      ProofUtil.fastforceTac,
      ProofUtil.cutTac(endToEndData.axiomSatLemma),
      ProofUtil.simpTac(ctxtBplDefLemmaName),
      ProofUtil.simp,
      ProofUtil.simpTac(Seq(translationRecord0DefLemma, endToEndData.programTotalProgEqLemma)),
    )
  }

  private val disjointGlobalDeclarationsTac = ProofUtil.ruleTac(ProofUtil.OF("disjoint_list_helper_lemma_7_to_4", "disjoint_property_aux"))

  private def disjointGlobalDeclarationsProof(): Seq[String] = {
    Seq(
      disjointGlobalDeclarationsTac,
      ProofUtil.simpTac
    )
  }

}
