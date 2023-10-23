package viper.carbon.proofgen.end_to_end

import isabelle.ast.{AbbrevDecl, ContextElem, DeclareDecl, DefDecl, IsaTermUtil, IsaThmUtil, IsaUtil, Lambda, LemmaDecl, NatConst, OuterDecl, Proof, ProofUtil, Term, TermApp, TermBinary, TermIdent, TermList, TermQuantifier, TermSet, TermTuple, Theory, VarType}
import viper.carbon.proofgen.hints.{AxiomTacticInput, BoogieAxiomProofHint, BoogieDeclProofHint, BoogieFuncProofHint}
import viper.carbon.proofgen.{BoogieConstGlobal, BoogieIsaTerm, EmptyFrameConst, FieldConst, FullPermConst, IsaBoogieGlobalAccessor, IsaViperGlobalDataAccessor, NoPermConst, NullConst, TypeRepresentation, ViperBoogieRelationIsa, ViperProgramRecord, ViperTotalContext, ZeroMaskConst, ZeroPMaskConst}

import scala.collection.mutable.ListBuffer


object EndToEndGlobalDataHelper {

  val typeInterpBplName = "type_interp_bpl"

  /**
    * Initializes the names of the global end-to-end data theory file. This allows decoupling theory files that need
    * to refer to data in the global end-to-end theory file from the actual generation of the global end-to-end theory file.
    * This is important, because the global end-to-end theory file can only be generated at the end, since the Boogie
    * declaration preamble (containing functions and axioms) is generated at the end.
    * @param theoryName
    * @return
    */
  def generateEndToEndData(theoryName: String): DefaultIsaViperEndToEndGlobalData = {
    DefaultIsaViperEndToEndGlobalData(
      theoryName = theoryName,
      ctxtVprName = "ctxt_vpr",
      abstractValueType = VarType("a"),
      programTotalProgEqLemmaName = "program_total_eq",
      funInterpInstData = FunInterpInstantiationData(
        funInterpVprBpl = "has_direct_perm_fun_interp_single_wf",
        funInterpVprBplWfLemma = "fun_interp_vpr_bpl_inst_wf",
        funInterpBplWfLemma = "fun_interp_wf"
      ),
      constantsData = ConstantsData("lookup_constants", "lookup_constants_with_globals"),
      fieldRelInstData = FieldRelInstantiationData(
        ranFieldRelLemma = "field_rel_ran",
        injFieldRelLemma = "inj_field_rel",
        fieldTrPropNoGlobalsLemma = "field_prop_aux_no_globals",
        fieldTrPropWithGlobalsLemma = "field_prop_aux_with_globals",
        declaredFieldsFieldRemDomEqLemma = "fields_dom_eq"
      ),
      axiomSatLemmaName = "axiom_sat"
    )
  }

  /**
    * Creates a theory file that contains declarations to be used by the end-to-end proof theories for each method.
    * @param endToEndData names to be used in the theory that are accessed by other theories
    * @param vprProgAccessor
    * @param bplGlobalAccessor
    * @param declHints Hints for all functions and axioms in the Boogie program. The order of the hints must match the order
    *                  in which the declarations appear in the Boogie program.
    * @param boogieProofDirName
    * @return
    */
  def generateEndToEndTheory(endToEndData: DefaultIsaViperEndToEndGlobalData,
                             vprProgAccessor: IsaViperGlobalDataAccessor, bplGlobalAccessor: IsaBoogieGlobalAccessor,
                             declHints: Seq[BoogieDeclProofHint],
                             boogieProofDirName: String) : Theory= {

    val funDeclHints = declHints.collect { case h: BoogieFuncProofHint => h }
    val axiomDeclHints = declHints.collect { case h: BoogieAxiomProofHint => h }

    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    outerDecls += DeclareDecl("Nat.One_nat_def[simp del]")
    outerDecls += DeclareDecl(s"${ViperTotalContext.totalContextRecordName}.defs(1)[simp]")
    outerDecls += DeclareDecl("program.defs(1)[simp]")


    outerDecls += BoogieIsaTerm.typeInterpBplAbbrev(typeInterpBplName)

    val vprContextDef : DefDecl = DefDecl(endToEndData.ctxtVprName, None,
      (Seq(), ViperTotalContext.makeTotalContext(
          program = vprProgAccessor.vprProgram,
          funInterp = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), IsaTermUtil.none),
          absvalInterp = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), IsaTermUtil.undefined))
      )
    )

    val vprContextTerm = TermIdent(vprContextDef.name)

    outerDecls += vprContextDef

    val programTotalEqLemma = LemmaDecl(endToEndData.programTotalProgEqLemmaName, TermBinary.eq(ViperTotalContext.programTotal(vprContextTerm), vprProgAccessor.vprProgram),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprContextDef.name))))))

    outerDecls += programTotalEqLemma

    val funInterpVprBplInstDef = DefDecl(endToEndData.funInterpInstData.funInterpVprBpl, None,
      (Seq(TermIdent("f")), ViperBoogieRelationIsa.funInterpVprBplConcrete(
        vprProgram = vprProgAccessor.vprProgram,
        typeRepresentation = TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        fieldMap = IsaTermUtil.mapOf(vprProgAccessor.fieldRel),
        funMap = TermIdent(ViperBoogieRelationIsa.funReprConcreteName),
        fun = TermIdent("f"))
      )
    )

    outerDecls += funInterpVprBplInstDef

    val funInterpVprBplInstWfLemma = LemmaDecl(endToEndData.funInterpInstData.funInterpVprBplWfLemma,
      ViperBoogieRelationIsa.funInterpVprBplWf(
        vprProgram = vprProgAccessor.vprProgram,
        typeRepresentation = TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        fieldMap = IsaTermUtil.mapOf(vprProgAccessor.fieldRel),
        funMap = TermIdent(ViperBoogieRelationIsa.funReprConcreteName),
        funInterpBpl = TermIdent(funInterpVprBplInstDef.name)
      ),
      Proof(
        Seq(
          ProofUtil.applyTac(ProofUtil.unfoldTac(IsaUtil.definitionLemmaFromName(funInterpVprBplInstDef.name))),
          ProofUtil.applyTac(ProofUtil.ruleTac(ViperBoogieRelationIsa.funInterpVprBplConcreteWfLemmaName)),
          ProofUtil.byTac(ProofUtil.ruleTac(ViperBoogieRelationIsa.funReprConcreteInjLemmaName))
        )
      )
    )

    outerDecls += funInterpVprBplInstWfLemma

    val funInterpWfLemma = generateFunInterpWfLemma(
      lemmaName = endToEndData.funInterpInstData.funInterpBplWfLemma,
      vprContextTerm = vprContextTerm,
      funInterpVprBplInstDef = funInterpVprBplInstDef.name,
      bplGlobalAccessor = bplGlobalAccessor,
      hints = funDeclHints
    )

    outerDecls += funInterpWfLemma

    val lookupConstantsNoGlobalsLemma = lookupConstantsLemma(
      lemmaName = endToEndData.constantsData.lookupConstantsNoGlobalsLemma, varDeclList = bplGlobalAccessor.constDecls,
      vprContext = vprContextTerm, constToMapOfThm = bplGlobalAccessor.getConstantWithoutGlobalsMapOfThm)

    outerDecls += lookupConstantsNoGlobalsLemma

    val lookupConstantsWithGlobalsLemma = lookupConstantsLemma(
      lemmaName = endToEndData.constantsData.constantsLookupWithGlobalsLemma, varDeclList = IsaTermUtil.appendList(bplGlobalAccessor.constDecls, bplGlobalAccessor.globalDecls),
      vprContext = vprContextTerm, constToMapOfThm = bplGlobalAccessor.getGlobalMapOfThm)

    outerDecls += lookupConstantsWithGlobalsLemma

    val ranFieldRelationLemma = LemmaDecl(
      endToEndData.fieldRelInstData.ranFieldRelLemma,
      TermBinary.eq(
        IsaTermUtil.ran(IsaTermUtil.mapOf(vprProgAccessor.fieldRel)),
        TermSet(vprProgAccessor.origProgram.fields.map(f => NatConst(bplGlobalAccessor.getVarId(FieldConst(f)))))
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString)))))
    )

    outerDecls += ranFieldRelationLemma

    val injFieldRelationLemma = LemmaDecl(
      endToEndData.fieldRelInstData.injFieldRelLemma,
      IsaTermUtil.injectiveOnDom(IsaTermUtil.mapOf(vprProgAccessor.fieldRel), IsaTermUtil.domainOfPartialFun(IsaTermUtil.mapOf(vprProgAccessor.fieldRel))),
      Proof(Seq(
        ProofUtil.applyTac(ProofUtil.ruleTac(IsaThmUtil.strictlyOrderedListInjMapOfLemma)),
        ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString)))
      ))
    )

    outerDecls += injFieldRelationLemma

    val fieldPropNoGlobalsLemmaName = endToEndData.fieldRelInstData.fieldTrPropNoGlobalsLemma
    val fieldPropNoGlobalsDecls = fieldPropListAll2Lemma(fieldPropNoGlobalsLemmaName, vprProgAccessor, programTotalEqLemma.name,
      vprContextTerm, bplGlobalAccessor.constDecls, bplGlobalAccessor.getConstantWithoutGlobalsMapOfThm)

    outerDecls.addAll(fieldPropNoGlobalsDecls)

    val fieldPropWithGlobalsLemmaName = endToEndData.fieldRelInstData.fieldTrPropWithGlobalsLemma
    val fieldPropWithGlobalsDecls = fieldPropListAll2Lemma(fieldPropWithGlobalsLemmaName, vprProgAccessor, programTotalEqLemma.name,
      vprContextTerm, IsaTermUtil.appendList(bplGlobalAccessor.constDecls, bplGlobalAccessor.globalDecls), bplGlobalAccessor.getGlobalMapOfThm)

    outerDecls.addAll(fieldPropWithGlobalsDecls)

    val domEqLemmaName = endToEndData.fieldRelInstData.declaredFieldsFieldRemDomEqLemma
    val domEqLemma = declaredFieldsFieldRelDomEqLemma(domEqLemmaName, vprProgAccessor, vprContextTerm, programTotalEqLemma.name)

    outerDecls += domEqLemma

    val axiomsSatLemma = generateAxiomSatLemma(
      lemmaName = endToEndData.axiomSatLemmaName,
      vprProgAccessor = vprProgAccessor,
      vprContextTerm = vprContextTerm,
      bplGlobalAccessor = bplGlobalAccessor,
      funInterpVprBplInstDef = funInterpVprBplInstDef.name,
      lookupFieldLemmas = vprProgAccessor.allFieldLookupLemmas,
      hints = axiomDeclHints
    )

    outerDecls += axiomsSatLemma

    Theory(endToEndData.theoryName,
      Seq(vprProgAccessor.theoryName, boogieProofDirName+"/"+bplGlobalAccessor.theoryName, "TotalViper.ViperBoogieEndToEndML"),
      outerDecls.toSeq)
  }

  private def lookupConstantsLemma(lemmaName: String, varDeclList: Term, vprContext: Term, constToMapOfThm: BoogieConstGlobal => String) : LemmaDecl = {
    LemmaDecl(lemmaName,
      TermBinary.eq(
        BoogieIsaTerm.lookupVarDeclsTy(varDeclList, TermApp(ViperBoogieRelationIsa.constReprBasic, TermIdent("c"))),
        IsaTermUtil.some(ViperBoogieRelationIsa.boogieConstTy(TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContext)), TermIdent("c")))
      ),
      Proof(
        ProofUtil.mapApplyTac(
          "cases c" +:
            ViperBoogieRelationIsa.boogieConstDataOrder.map(
              boogieConst =>
                ProofUtil.simpTac(
                  Seq(ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, constToMapOfThm(boogieConst)),
                    IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName)
                  )
                )
            )
        ) :+
        ProofUtil.doneTac
      )
    )
  }

  private def fieldPropListAll2Lemma( lemmaName: String,
                                      vprProgAccessor: IsaViperGlobalDataAccessor,
                                      programTotalEqLemma: String,
                                      vprContextTerm: Term,
                                      globalsBplVarDecls: Term,
                                      constToMapOfThm: BoogieConstGlobal => String ) : Seq[OuterDecl]= {
    val helperLemma =
      LemmaDecl(lemmaName+"list_all2",
        IsaTermUtil.listAll2(TermApp(TermIdent("field_tr_prop"),
              TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
              globalsBplVarDecls
          ), vprProgAccessor.fields, vprProgAccessor.fieldRel
        ), Proof(
          Seq(
            ProofUtil.applyTac(ProofUtil.simpTacOnly(
              Seq(
                IsaUtil.definitionLemmaFromName(vprProgAccessor.fields.id.toString),
                IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString),
              )
            )),
            ProofUtil.byTac(ProofUtil.simpTac(
              Seq(
                IsaUtil.definitionLemmaFromName("field_tr_prop"),
                IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName),
              ) ++
                vprProgAccessor.origProgram.fields.map(f =>
                  ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, constToMapOfThm(FieldConst(f)))
                )
             )
            )
          )
        )
      )

    val mainLemma = LemmaDecl(lemmaName,
        TermApp(TermIdent("field_tr_prop_full"), Seq(
          ViperTotalContext.programTotal(vprContextTerm),
          globalsBplVarDecls,
          TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
          IsaTermUtil.mapOf(vprProgAccessor.fieldRel)
        )),
        Proof(
          ProofUtil.mapApplyTac(
            Seq(
              ProofUtil.simpTacOnly(IsaUtil.definitionLemmaFromName("field_tr_prop_full")),
              ProofUtil.repeatTac("rule allI | rule impI"),
              ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName(vprProgAccessor.vprProgram.id.toString), programTotalEqLemma))
            )
          ) :+
          ProofUtil.byTac(ProofUtil.eruleTac(ProofUtil.OF(IsaThmUtil.listAll2MapOf, Seq(helperLemma.name, "field_tr_prop_fst"))))
        )
      )

    Seq(helperLemma, mainLemma)
  }

  private def declaredFieldsFieldRelDomEqLemma(lemmaName: String, vprProgAccessor: IsaViperGlobalDataAccessor, vprcontextTerm: Term, programTotalEqLemma: String) : LemmaDecl = {
    val statement = TermBinary.eq(IsaTermUtil.domainOfPartialFun(ViperProgramRecord.fields(ViperTotalContext.programTotal(vprcontextTerm))),
                                  IsaTermUtil.domainOfPartialFun(IsaTermUtil.mapOf(vprProgAccessor.fieldRel)))

    LemmaDecl(lemmaName, statement,
      Proof(Seq(
        ProofUtil.applyTac(ProofUtil.simpTac(Seq(programTotalEqLemma, IsaUtil.definitionLemmaFromName(vprProgAccessor.vprProgram.toString)))),
        ProofUtil.byTac(
          ProofUtil.simpTac(Seq("dom_map_of_2", IsaUtil.definitionLemmaFromName(vprProgAccessor.fields.toString), IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.toString)))
        )
      ))
    )
  }

  private def generateFunInterpWfLemma(lemmaName: String, vprContextTerm: Term, bplGlobalAccessor: IsaBoogieGlobalAccessor, funInterpVprBplInstDef: String, hints: Seq[BoogieFuncProofHint]) : LemmaDecl = {
    val initTacticsWithoutApply =
      Seq(
        ProofUtil.simpTacOnly(IsaUtil.definitionLemmaFromName(BoogieIsaTerm.funInterpWfId.toString)),
        ProofUtil.ruleTac(IsaThmUtil.listAllMapOf),
        ProofUtil.simpTacOnly(Seq(IsaUtil.definitionLemmaFromName(bplGlobalAccessor.funDecls.toString), "List.list_all_simps(1)")),
        ProofUtil.simpTacDel(Seq(IsaUtil.simpsOfFun(BoogieIsaTerm.funInterpSingleWfId.toString), IsaUtil.simpsOfFun(BoogieIsaTerm.funInterpSingleWf2Id.toString))),
        ProofUtil.simpTacOnly(IsaUtil.definitionLemmaFromName(funInterpVprBplInstDef)),
        ProofUtil.introTac("conjI")
      )

    val funWfTactics =
        hints.map(h => h.funInterpWellFormednessProof()).flatten

    LemmaDecl(lemmaName,
      BoogieIsaTerm.funInterpWf(
        typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        funDecls = bplGlobalAccessor.funDecls,
        funInterp = TermIdent(funInterpVprBplInstDef)
      ),
      Proof(ProofUtil.mapApplyTac(initTacticsWithoutApply) ++ funWfTactics :+ ProofUtil.doneTac)
    )
  }

  private def generateAxiomSatLemma(lemmaName: String, vprProgAccessor: IsaViperGlobalDataAccessor, vprContextTerm: Term, bplGlobalAccessor: IsaBoogieGlobalAccessor, funInterpVprBplInstDef: String, lookupFieldLemmas: String, hints: Seq[BoogieAxiomProofHint]) : LemmaDecl = {
    val initTacticsWithoutApply =
      Seq(
        ProofUtil.simpTacOnly(Seq(IsaUtil.definitionLemmaFromName(bplGlobalAccessor.axiomDecls.toString), IsaUtil.definitionLemmaFromName(BoogieIsaTerm.axiomsSatId.toString))),
        ProofUtil.simp,
        ProofUtil.introTac("conjI")
      )
      
    val axiomTacticInput = AxiomTacticInput(
      vprProg = vprProgAccessor.origProgram,
      funInterpDef = funInterpVprBplInstDef,
      boogieConstRel = "assms(1)",
      fieldRel = "assms(2)",
      lookupFieldLemmas = lookupFieldLemmas,
      delThms = "axioms_sat_proof_del"
    )

    val axiomSatTactics = hints.map(h => h.satProof(axiomTacticInput)).flatten

    LemmaDecl(lemmaName,
      ContextElem.onlyAssumptionsNoLabels(Seq(
          ViperBoogieRelationIsa.boogieConstRel(
            constRepr = ViperBoogieRelationIsa.constReprBasic,
            varContext = TermTuple(bplGlobalAccessor.constDecls, TermList.empty),
            normalState = TermIdent("ns") ),
          ViperBoogieRelationIsa.fieldRel(
            vprProgram = vprProgAccessor.vprProgram,
            bplVarContext = TermTuple(bplGlobalAccessor.constDecls, TermList.empty),
            fieldMap = IsaTermUtil.mapOf(vprProgAccessor.fieldRel),
            normalState = TermIdent("ns")
          )
        )
      ),
      BoogieIsaTerm.axiomsSat(
        typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        varContext = TermTuple(bplGlobalAccessor.constDecls, TermList.empty),
        funInterp = TermIdent(funInterpVprBplInstDef),
        normalState = TermIdent("ns"),
        axioms = bplGlobalAccessor.axiomDecls
      ),
      Proof(
        ProofUtil.mapApplyTac(initTacticsWithoutApply) ++ axiomSatTactics :+ ProofUtil.doneTac
      )
    )
  }

}
