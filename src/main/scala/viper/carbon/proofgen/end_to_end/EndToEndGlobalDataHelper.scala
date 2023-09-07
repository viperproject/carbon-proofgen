package viper.carbon.proofgen.end_to_end

import isabelle.ast.{AbbrevDecl, DeclareDecl, DefDecl, IsaTermUtil, IsaUtil, Lambda, LemmaDecl, NatConst, OuterDecl, Proof, ProofUtil, TermApp, TermBinary, TermIdent, TermQuantifier, TermSet, Theory}
import viper.carbon.proofgen.{BoogieIsaTerm, FieldConst, IsaBoogieGlobalAccessor, IsaViperGlobalDataAccessor, TypeRepresentation, ViperBoogieRelationIsa, ViperTotalContext}

import scala.collection.mutable.ListBuffer


object EndToEndGlobalDataHelper {

  val typeInterpBplName = "type_interp_bpl"

  def generateEndToEndData(theoryName: String, vprProgAccessor: IsaViperGlobalDataAccessor, bplGlobalAccessor: IsaBoogieGlobalAccessor, boogieProofDirName: String) : (Theory, IsaViperEndToEndGlobalData) = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    outerDecls += DeclareDecl("Nat.One_nat_def[simp del]")
    outerDecls += DeclareDecl(s"${ViperTotalContext.totalContextRecordName}.defs(1)[simp]")


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

    val ranFieldRelationLemma = LemmaDecl(
      "field_rel_ran",
      TermBinary.eq(
        IsaTermUtil.ran(IsaTermUtil.mapOf(vprProgAccessor.fieldRel)),
        TermSet(vprProgAccessor.origProgram.fields.map(f => NatConst(bplGlobalAccessor.getVarId(FieldConst(f)))))
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(vprProgAccessor.fieldRel.id.toString)))))
    )

    outerDecls += ranFieldRelationLemma

    val fieldTrPropName = "field_tr_prop"

    val fieldPropLemma = LemmaDecl("field_prop_list_all2",
      IsaTermUtil.listAll2(TermApp(TermIdent(fieldTrPropName),
            TypeRepresentation.makeBasicTypeRepresentation(ViperTotalContext.absvalInterpTotal(vprContextTerm)),
            IsaTermUtil.appendList(bplGlobalAccessor.constDecls, bplGlobalAccessor.globalDecls)
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
              IsaUtil.definitionLemmaFromName(fieldTrPropName),
              IsaUtil.definitionLemmaFromName(TypeRepresentation.tyReprBasicName),
            ) ++
              vprProgAccessor.origProgram.fields.map(f =>
                ProofUtil.OF(BoogieIsaTerm.mapOfLookupVarDeclsTyThm, bplGlobalAccessor.getGlobalMapOfThm(FieldConst(f)))
              )
           )
          )
        )
      )
    )

    outerDecls += fieldPropLemma

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
        fieldRelInstData = FieldRelInstantiationData(ranFieldRelLemma = ranFieldRelationLemma.name, fieldTrPropLemma = fieldPropLemma.name),
        axiomSatLemmaName = "TODO"
      )

    (theory, data)
  }

}
