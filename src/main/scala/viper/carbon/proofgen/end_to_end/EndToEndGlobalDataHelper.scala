package viper.carbon.proofgen.end_to_end

import isabelle.ast.{AbbrevDecl, ContextElem, DeclareDecl, DefDecl, IsaTermUtil, IsaThmUtil, IsaUtil, Lambda, LemmaDecl, NatConst, OuterDecl, Proof, ProofUtil, Term, TermApp, TermBinary, TermIdent, TermList, TermQuantifier, TermSet, TermTuple, Theory}
import viper.carbon.proofgen.{BoogieConstGlobal, BoogieIsaTerm, EmptyFrameConst, FieldConst, FullPermConst, IsaBoogieGlobalAccessor, IsaViperGlobalDataAccessor, NoPermConst, NullConst, TypeRepresentation, ViperBoogieRelationIsa, ViperTotalContext, ZeroMaskConst, ZeroPMaskConst}

import scala.collection.mutable.ListBuffer


object EndToEndGlobalDataHelper {

  val typeInterpBplName = "type_interp_bpl"

  def generateEndToEndData(theoryName: String, vprProgAccessor: IsaViperGlobalDataAccessor, bplGlobalAccessor: IsaBoogieGlobalAccessor, boogieProofDirName: String) : (Theory, IsaViperEndToEndGlobalData) = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    outerDecls += DeclareDecl("Nat.One_nat_def[simp del]")
    outerDecls += DeclareDecl(s"${ViperTotalContext.totalContextRecordName}.defs(1)[simp]")
    outerDecls += DeclareDecl("program.defs(1)[simp]")


    outerDecls += BoogieIsaTerm.typeInterpBplAbbrev(typeInterpBplName)

    val vprContextDef : DefDecl = DefDecl("ctxt_vpr", None,
      (Seq(), ViperTotalContext.makeTotalContext(
          program = vprProgAccessor.vprProgram,
          funInterp = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), IsaTermUtil.none),
          absvalInterp = TermQuantifier(Lambda, Seq(isabelle.ast.Wildcard), IsaTermUtil.undefined))
      )
    )

    val vprContextTerm = TermIdent(vprContextDef.name)

    outerDecls += vprContextDef

    val programTotalEqLemma = LemmaDecl("program_total_vpr", TermBinary.eq(ViperTotalContext.programTotal(vprContextTerm), vprProgAccessor.vprProgram),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprContextDef.name))))))

    outerDecls += programTotalEqLemma

    val funInterpVprBplInstDef = DefDecl("fun_interp_vpr_bpl_inst", None,
      (Seq(TermIdent("f")), ViperBoogieRelationIsa.funInterpVprBplConcrete(
        vprProgram = vprProgAccessor.vprProgram,
        typeRepresentation = TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        fieldMap = IsaTermUtil.mapOf(vprProgAccessor.fieldRel),
        funMap = TermIdent(ViperBoogieRelationIsa.funReprConcreteName),
        fun = TermIdent("f"))
      )
    )

    outerDecls += funInterpVprBplInstDef

    val funInterpVprBplInstWfLemma = LemmaDecl("fun_interp_vpr_bpl_inst_wf",
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

    val funInterpWfLemma = LemmaDecl("fun_interp_wf",
      BoogieIsaTerm.funInterpWf(
        typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        funDecls = bplGlobalAccessor.funDecls,
        funInterp = TermIdent(funInterpVprBplInstDef.name)
      ),
      Proof(Seq("sorry"))
    )

    outerDecls += funInterpWfLemma

    val lookupConstantsNoGlobalsLemma = lookupConstantsLemma(
      lemmaName = "lookup_constants", varDeclList = bplGlobalAccessor.constDecls,
      vprContext = vprContextTerm, constToMapOfThm = bplGlobalAccessor.getConstantWithoutGlobalsMapOfThm)

    outerDecls += lookupConstantsNoGlobalsLemma

    val lookupConstantsWithGlobalsLemma = lookupConstantsLemma(
      lemmaName = "lookup_constants_with_globals", varDeclList = IsaTermUtil.appendList(bplGlobalAccessor.constDecls, bplGlobalAccessor.globalDecls),
      vprContext = vprContextTerm, constToMapOfThm = bplGlobalAccessor.getGlobalMapOfThm)

    outerDecls += lookupConstantsWithGlobalsLemma

    val ranFieldRelationLemma = LemmaDecl(
      "field_rel_ran",
      TermBinary.eq(
        IsaTermUtil.ran(IsaTermUtil.mapOf(vprProgAccessor.fieldRel)),
        TermSet(vprProgAccessor.origProgram.fields.map(f => NatConst(bplGlobalAccessor.getVarId(FieldConst(f)))))
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString)))))
    )

    outerDecls += ranFieldRelationLemma

    val injFieldRelationLemma = LemmaDecl(
      "inj_field_rel",
      IsaTermUtil.injectiveOnDom(IsaTermUtil.mapOf(vprProgAccessor.fieldRel), IsaTermUtil.domainOfPartialFun(IsaTermUtil.mapOf(vprProgAccessor.fieldRel))),
      Proof(Seq(
        ProofUtil.applyTac(ProofUtil.ruleTac(IsaThmUtil.strictlyOrderedListInjMapOfLemma)),
        ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString)))
      ))
    )

    outerDecls += injFieldRelationLemma

    val fieldPropNoGlobalsLemmaName = "field_prop_aux_no_globals"
    val fieldPropNoGlobalsDecls = fieldPropListAll2Lemma(fieldPropNoGlobalsLemmaName, vprProgAccessor, programTotalEqLemma.name,
      vprContextTerm, bplGlobalAccessor.constDecls, bplGlobalAccessor.getConstantWithoutGlobalsMapOfThm)

    outerDecls.addAll(fieldPropNoGlobalsDecls)


    val fieldPropWithGlobalsLemmaName = "field_prop_aux_with_globals"
    val fieldPropWithGlobalsDecls = fieldPropListAll2Lemma(fieldPropWithGlobalsLemmaName, vprProgAccessor, programTotalEqLemma.name,
      vprContextTerm, IsaTermUtil.appendList(bplGlobalAccessor.constDecls, bplGlobalAccessor.globalDecls), bplGlobalAccessor.getGlobalMapOfThm)

    outerDecls.addAll(fieldPropWithGlobalsDecls)

    val axiomsSatLemma = LemmaDecl("axioms_sat",
      ContextElem.onlyAssumptionsNoLabels(Seq(ViperBoogieRelationIsa.boogieConstRel(
        constRepr = ViperBoogieRelationIsa.constReprBasic,
        varContext = TermTuple(bplGlobalAccessor.constDecls, TermList.empty),
        normalState = TermIdent("ns")))),
      BoogieIsaTerm.axiomsSat(
        typeInterp = TermApp(TermIdent(typeInterpBplName), ViperTotalContext.absvalInterpTotal(vprContextTerm)),
        varContext = TermTuple(bplGlobalAccessor.constDecls, TermList.empty),
        funInterp = TermIdent(funInterpVprBplInstDef.name),
        normalState = TermIdent("ns"),
        axioms = bplGlobalAccessor.axiomDecls
      ),
      Proof(Seq("sorry"))
    )

    outerDecls += axiomsSatLemma

    val theory = Theory(theoryName,
      Seq(vprProgAccessor.theoryName, boogieProofDirName+"/"+bplGlobalAccessor.theoryName, "TotalViper.ViperBoogieEndToEndML"),
      outerDecls.toSeq)

    val data =
      DefaultIsaViperEndToEndGlobalData(
        theoryName = theoryName,
        ctxtVprName = vprContextDef.name,
        programTotalProgEqLemmaName = programTotalEqLemma.name,
        funInterpInstData = FunInterpInstantiationData(
          funInterpVprBpl = funInterpVprBplInstDef.name,
          funInterpVprBplWfLemma = funInterpVprBplInstWfLemma.name,
          funInterpBplWfLemma = funInterpWfLemma.name
        ),
        constantsData = ConstantsData(lookupConstantsNoGlobalsLemma.name, lookupConstantsWithGlobalsLemma.name),
        fieldRelInstData = FieldRelInstantiationData(
          ranFieldRelLemma = ranFieldRelationLemma.name,
          injFieldRelLemma = injFieldRelationLemma.name,
          fieldTrPropNoGlobalsLemma = fieldPropNoGlobalsLemmaName,
          fieldTrPropWithGlobalsLemma = fieldPropWithGlobalsLemmaName
        ),
        axiomSatLemmaName = axiomsSatLemma.name
      )

    (theory, data)
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
          "done"
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

}
