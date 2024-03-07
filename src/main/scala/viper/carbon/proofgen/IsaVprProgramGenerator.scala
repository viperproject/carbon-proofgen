package viper.carbon.proofgen

import isabelle.ast.{AbbrevDecl, ContextElem, DefDecl, IsaTermUtil, IsaTypeUtil, IsaUtil, LemmaDecl, LemmasDecl, MLDecl, MLNormal, MLUtil, NatConst, OuterDecl, Proof, ProofUtil, StringConst, Term, TermApp, TermBinary, TermIdent, TermList, TermSet, TermTuple, Theory, TupleType, TypeIsa}
import viper.silver.{ast => sil}
import viper.carbon.proofgen.util.StringBuilderExtension._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object IsaVprProgramGenerator {

  def globalData(p: sil.Program, boogieGlobalAccessor: IsaBoogieGlobalAccessor, theoryName: String, pathToTheoryDir: String) : (Theory, IsaViperGlobalDataAccessor, Seq[Theory]) =
  {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

    val fieldToTerm : Map[sil.Field, Term] = p.fields.map(f => (f, StringConst(f.name))).toMap

    val fieldsList = TermList(p.fields.map(f => TermTuple(fieldToTerm.get(f).get, ViperIsaType.translate(f.typ))))

    val fieldsListDef = DefDecl("field_list", None, (Seq(), fieldsList))
    outerDecls += fieldsListDef

    val fieldRelationList = {
      //we need to sort the values such that we can prove injectivity via an ordering lemma
      TermList(
        p.fields.map(f => {
          val vprFieldConstTerm = fieldToTerm.get(f).get
          val bplFieldConstId = boogieGlobalAccessor.getVarId(FieldConst(f))
          (vprFieldConstTerm, bplFieldConstId)
        }).sortBy(a => a._2)
          .map( {
            case (vprFieldConstTerm, bplFieldConstId) =>
              TermTuple(vprFieldConstTerm, NatConst(bplFieldConstId))
          })
        )
    }

    val fieldRelationListDef = DefDecl(
      "field_rel_list",
      Some(IsaTypeUtil.listType(TupleType(IsaTypeUtil.stringType, BoogieIsaType.varNameType))),
      (Seq(), fieldRelationList)
    )

    outerDecls += fieldRelationListDef

    val fieldRelationBoundedBy = LemmaDecl(
      "field_rel_bound",
      ViperBoogieIsaUtil.allVarsInListBoundedBy(
        TermIdent(fieldRelationListDef.name),
        ViperBoogieIsaUtil.minInRangeOfList(fieldRelationList),
        ViperBoogieIsaUtil.maxInRangeOfList(fieldRelationList)
      ),
      Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(fieldRelationListDef.name)))))
    )

    outerDecls += fieldRelationBoundedBy

    val (methodTheory, allMethodsAccessor) = methodsRepr("method_decls", p)

    val programDef = DefDecl("vpr_prog",
      Some(ViperIsaType.vprProgramType),
      (Seq(), IsaTermUtil.makeRecord(ViperIsaType.vprProgramTypeName, Seq(
        allMethodsAccessor.methodLookupFun,
        TermIdent("f_None"),//TODO: predicates
        TermIdent("f_None"),//TODO: functions
        IsaTermUtil.mapOf(TermIdent(fieldsListDef.name)), //functions
        NatConst(0)) //domains
      ))
    )

    outerDecls += programDef

    val fieldRelMapOfLemmasName = "field_rel_map_of_lemmas"
    val fieldRelLookupLemmasName = "field_rel_lookup_lemmas"
    val allFieldLookupLemmas = AllFieldLookupLemmas(fieldRelMapOfLemmasName, fieldRelLookupLemmasName)

    val fieldRelLookupDecls = generateFieldRelLookupLemmas(
      (fieldRelMapOfLemmasName, fieldRelLookupLemmasName),
      vprProgramTerm = TermIdent(programDef.name),
      fields = p.fields,
      fieldsListDef = TermIdent(fieldsListDef.name),
      fieldRelListDef = TermIdent(fieldRelationListDef.name),
      boogieGlobalAccessor = boogieGlobalAccessor
    )

    outerDecls.addAll(fieldRelLookupDecls)

    val methodProjThm = LemmaDecl(
      "methods_vpr_prog",
      TermBinary.eq(ViperProgramRecord.methods(TermIdent(programDef.name)), allMethodsAccessor.methodLookupFun),
      Proof(
        Seq(ProofUtil.byTac(ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName(programDef.name), "ViperLang.program.defs(1)"))))
      ))

    outerDecls += methodProjThm

    val methodDataName = "method_decl_data"

    outerDecls += methodDataML(methodDataName, p.methods, allMethodsAccessor, methodProjThm.name)

    (
      //ViperBoogieTranslationInterface is required for the field lookup lemmas, could think about putting those in a separate theory
      Theory(theoryName, Seq("ViperCommon.ViperLang", "TotalViper.TotalViperUtil", "TotalViper.TotalViperHelperML", "TotalViper.ViperBoogieTranslationInterface",  methodTheory.theoryName), outerDecls.toSeq),
      DefaultIsaViperGlobalDataAccessor(
        theoryName = theoryName,
        vprProgramIdent = programDef.name,
        origProgram = p,
        fieldsIdent = fieldsListDef.name,
        fieldToTerm = fieldToTerm,
        fieldRelIdent = fieldRelationListDef.name,
        fieldRelBoundedLemma = fieldRelationBoundedBy.name,
        allFieldLookupLemmas = allFieldLookupLemmas,
        methodsProgEqLemmaName = methodProjThm.name,
        allMethodsAccessor = allMethodsAccessor,
        methodDataTableML = methodDataName
      ),
      Seq(methodTheory)
    )
  }

  private def mapOfFieldLemmaName(f: sil.Field) : String = s"mfield_${IsaUtil.convertToValidIsabelleIdentifier(f.name)}"

  private def lookupFieldLemmaName(f: sil.Field) : String = s"lfield_${IsaUtil.convertToValidIsabelleIdentifier(f.name)}"

  private def generateFieldRelLookupLemmas(mapOfAndlookupLemmasName: (String, String), vprProgramTerm: TermIdent, fields: Seq[sil.Field], fieldsListDef: TermIdent, fieldRelListDef: TermIdent, boogieGlobalAccessor: IsaBoogieGlobalAccessor) : Seq[OuterDecl] =
  {
    val varContextBpl = TermIdent("VarC")
    val normalState = TermIdent("ns")

    val fieldRelAssumption = ViperBoogieRelationIsa.fieldRel(vprProgramTerm, varContextBpl, IsaTermUtil.mapOf(fieldRelListDef), normalState)


    val decls =
      for(f <- fields) yield {
        val boogieVarId = NatConst(boogieGlobalAccessor.getVarId(FieldConst(f)))

        val mapOfLemma : OuterDecl =
          LemmaDecl(mapOfFieldLemmaName(f),
            TermBinary.eq(TermApp(IsaTermUtil.mapOf(fieldRelListDef), StringConst(f.name)), IsaTermUtil.some(boogieVarId)),
            Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(fieldRelListDef.toString)))))
          )

        val lookupLemma =
          LemmaDecl(lookupFieldLemmaName(f),
            ContextElem.onlyAssumptions(Seq((Some("FieldRel"), fieldRelAssumption))),
            TermBinary.eq(BoogieIsaTerm.lookupVar(varContextBpl, normalState, boogieVarId),
              IsaTermUtil.some(BoogieIsaTerm.normalFieldAbstractValue(boogieVarId, ViperIsaType.translate(f.typ)))),
            Proof(
              Seq(
                ProofUtil.applyTac(ProofUtil.ruleTac(ProofUtil.OF("lookup_field_rel", Seq("FieldRel", mapOfLemma.name)))),
                ProofUtil.byTac(ProofUtil.simpTac(
                  Seq(IsaUtil.definitionLemmaFromName(vprProgramTerm.toString),
                    IsaUtil.definitionLemmaFromName(fieldsListDef.toString),
                    "program.defs(1)"))
                )
              )
            )
          )

        Seq(mapOfLemma, lookupLemma)
      }


    val (mapOfLemmasName, lookupLemmasName) =  mapOfAndlookupLemmasName

    decls.flatten :+
      LemmasDecl(mapOfLemmasName,
        //if there are no fields, then just add a placeholder lemma (need at least one lemma for the declaration to be valid)
        if (fields.isEmpty) { Seq("HOL.refl") } else { fields.map(mapOfFieldLemmaName) }
      ) :+
      LemmasDecl(lookupLemmasName,
        //if there are no fields, then just add a placeholder lemma (need at least one lemma for the declaration to be valid)
        if(fields.isEmpty) { Seq("HOL.refl") } else { fields.map(lookupFieldLemmaName) }
      )
  }


  private def methodDataML(methodDataName: String, methods: Seq[sil.Method], allMethodsAccessor: IsaViperAllMethodsAccessor, vprProgMethodsProjThm: String) : OuterDecl =
  {
    def variableName(methodName: String) =  s"${methodName}_data"

    val sb = new StringBuilder
    for (m <- methods) {
      sb.append(s"val ${variableName(m.name)} = ").append(
        methodDataRecord(allMethodsAccessor.methodAccessor(m.name), allMethodsAccessor.lookupLemmaName(m.name), vprProgMethodsProjThm)
      ).newLine
    }


    sb.append(s"val $methodDataName : method_data Symtab.table = ").newLine

    sb.append("(")

    val tableUpdates = methods.map(m => MLUtil.app("Symtab.update", MLUtil.createTuple(Seq(MLUtil.createString(m.name), variableName(m.name)))))

    sb.append(("Symtab.empty" +: tableUpdates).mkString("|>"))
    sb.append(")")

    MLDecl(Seq(sb.toString()), MLNormal)
  }

  private def methodDataRecord(methodAccessor: IsaViperMethodAccessor, methodLookupThm: String, vprProgMethodsProjThm: String) : String =
  {
    MLUtil.createRecord(
      Seq(
        ("method_arg_thm", MLUtil.isaToMLThm(methodAccessor.methodDeclProjectionLemmaName(IsaMethodArgTypes))),
        ("method_rets_thm", MLUtil.isaToMLThm(methodAccessor.methodDeclProjectionLemmaName(IsaMethodRetTypes))),
        ("method_pre_thm", MLUtil.isaToMLThm(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPrecondition))),
        ("method_post_thm", MLUtil.isaToMLThm(methodAccessor.methodDeclProjectionLemmaName(IsaMethodPostcondition))),
        ("method_lookup_thm", MLUtil.isaToMLThm(ProofUtil.simplified(methodLookupThm, ProofUtil.OF("HOL.sym", vprProgMethodsProjThm))))
      )
    )
  }

  private def methodsRepr(theoryName: String, p: sil.Program) : (Theory, IsaViperAllMethodsAccessor) = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty
    val map: mutable.Map[String, IsaViperMethodAccessor] = mutable.Map.empty

    for(m <- p.methods) {
      //TODO: put variable translation in a common place
      val varTranslation = DeBruijnTranslation.freshTranslation((m.formalArgs ++ m.formalReturns) map (varDecl => varDecl.localVar))

      val (mOuterDecls, mAccessor) = methodProgramRepr(m, theoryName, varTranslation, p)
      outerDecls.addAll(mOuterDecls)
      map.put(m.name, mAccessor)
    }

    val methodOrder = p.methods

    /** It is important that the association list reflects the order that is eventually specified by [[IsaViperAllMethodsAccessor.methodOrder]] */
    val associationList = TermList(methodOrder.map( m => {
      val (methodName, methodAccessor) = (m.name, map.get(m.name).get)
      TermTuple(StringConst(methodName), methodAccessor.methodDecl)
    }))

    val methodLookupDef = DefDecl("methodLookupFun", None, (Seq(), IsaTermUtil.mapOf(associationList)))
    outerDecls += methodLookupDef

    map.foreach(
      {
        case (methodName, methodsAccessor) =>
          val lookupLemma = {
            LemmaDecl(
              DefaultIsaViperAllMethodsAccessor.lookupLemmaName(methodName),
              TermBinary.eq(TermApp(TermIdent(methodLookupDef.name), StringConst(methodName)), IsaTermUtil.some(methodsAccessor.methodDecl)),
              Proof(
                Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(methodLookupDef.name))))
              )
            )
          }

          outerDecls += lookupLemma
      }
    )

    val allMethodsAccessor = DefaultIsaViperAllMethodsAccessor(
      theoryName = theoryName,
      methodOrder = methodOrder,
      methodLookupFun = TermIdent(methodLookupDef.name),
      methodAccessorMap = map.toMap
    )

    val theory = Theory(theoryName, Seq("ViperCommon.ViperLang"), outerDecls.toSeq)
    (theory, allMethodsAccessor)
  }

  private def translateVarDecl(varTranslation: VarTranslation[sil.LocalVar], varDecl: sil.LocalVarDecl) : (Term, Term) = {
    varTranslation.translateVariableId(varDecl.localVar) match {
      case Some(id) =>
        (TermIdent(id.toString), ViperIsaType.translate(varDecl.typ))
      case None => sys.error(s"could not translate local variable ${varDecl.localVar.toString()}")
    }
  }

  //m must not be abstract
  private def methodProgramRepr(m: sil.Method, theoryName: String, varTranslation: VarTranslation[sil.LocalVar], vprProg: sil.Program) : (Seq[OuterDecl], IsaViperMethodAccessor) =
  {
    def outerDeclName(name: String) = s"${m.name}_$name"

    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty
    val methodDeclMemberToResultAndDef : mutable.Map[MethodDeclMember, (Term, Option[String])] = mutable.Map.empty

    val mBodyTerm =
      m.body match {
        case Some(bodyValue) => IsaTermUtil.some(ViperToIsa.translateStmt(bodyValue)(varTranslation))
        case None => IsaTermUtil.none
      }

    val mBodyDecl = DefDecl(outerDeclName("body"), Some(IsaTypeUtil.optionType(ViperIsaType.stmt)), (Seq(), mBodyTerm))
    outerDecls += mBodyDecl

    methodDeclMemberToResultAndDef.put(IsaMethodBody, (mBodyTerm, Some(IsaUtil.definitionLemmaFromName(mBodyDecl.name))))

    val mArgs = m.formalArgs.map(varDecl => translateVarDecl(varTranslation, varDecl))
    val mArgsTypesTerm = TermList(mArgs.map( { case (_, typ) => typ }))
    val mArgsTerm = TermList(mArgs.map( { case (id, typ) => TermTuple(id,typ)}))

    val mArgsDecl = AbbrevDecl(
      outerDeclName("args"),
      Some(IsaTypeUtil.listType( TupleType(Seq(ViperIsaType.varNameType, ViperIsaType.viperTyType)) )),
      (Seq(), mArgsTerm)
    )

    outerDecls += mArgsDecl
    methodDeclMemberToResultAndDef.put(IsaMethodArgTypes, (mArgsTypesTerm,None))

    val mReturns = m.formalReturns.map(varDecl => translateVarDecl(varTranslation, varDecl))
    val mReturnsTypesTerm = TermList(mReturns.map( { case (_, typ) => typ }))
    val mReturnsTerm = TermList(mReturns.map( { case (id, typ) => TermTuple(id,typ)}))

    val mReturnsDecl = AbbrevDecl(
      outerDeclName("returns"),
      Some(IsaTypeUtil.listType( TupleType(Seq(ViperIsaType.varNameType, ViperIsaType.viperTyType)) )),
      (Seq(), mReturnsTerm)
    )

    outerDecls += mReturnsDecl
    methodDeclMemberToResultAndDef.put(IsaMethodRetTypes, (mReturnsTypesTerm, None))

    val mPreTerm = ViperToIsa.translateAssertion(IsaMethodSpecificationHelper.conjoinSpecAssertions(m.pres))(varTranslation)
    val mPreDecl = DefDecl(outerDeclName("pre"), None, (Seq(), mPreTerm))

    outerDecls += mPreDecl
    methodDeclMemberToResultAndDef.put(IsaMethodPrecondition, (mPreTerm, Some(IsaUtil.definitionLemmaFromName(mPreDecl.name)))) //directly go to term instead of definition

    val mPostTerm = ViperToIsa.translateAssertion(IsaMethodSpecificationHelper.conjoinSpecAssertions(m.posts))(varTranslation)
    val mPostDecl = DefDecl(outerDeclName("post"), None, (Seq(), mPostTerm))

    outerDecls += mPostDecl
    methodDeclMemberToResultAndDef.put(IsaMethodPostcondition, (mPostTerm, Some(IsaUtil.definitionLemmaFromName(mPostDecl.name))))

    val methodDeclTerm = IsaMethodDecl.makeMethodDeclRecord(
      argTypes = mArgsTypesTerm,
      retTypes = mReturnsTypesTerm,
      precondition = TermIdent(mPreDecl.name),
      postcondition = TermIdent(mPostDecl.name),
      methodBody = TermIdent(mBodyDecl.name)
    )

    val methodDecl = DefDecl(outerDeclName("decl"), None, (Seq(), methodDeclTerm))

    outerDecls += methodDecl

    for (methodDeclMember <- IsaMethodDecl.allMethodDeclMembers) {
      val (result, defNameOpt) = methodDeclMemberToResultAndDef(methodDeclMember)
      outerDecls +=
        projectionLemma(
          DefaultIsaMethodAccessor.methodDeclProjectionLemmaName(m.name, methodDeclMember),
          IsaMethodDecl.methodDeclProjectionFunction(methodDeclMember),
          TermIdent(methodDecl.name),
          result,
          Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(Seq(IsaUtil.definitionLemmaFromName(methodDecl.name), "method_decl.defs(1)") ++ defNameOpt.fold[Seq[String]](Seq())(defName => Seq(defName))))))
        )
    }

    val mAccessor = DefaultIsaMethodAccessor(
      theoryName = theoryName,
      methodBodyIdent = mBodyDecl.name,
      methodArgsIdent = mArgsDecl.name,
      methodRetsIdent = mReturnsDecl.name,
      methodDeclIdent = methodDecl.name,
      origProgram = vprProg,
      origMethod = m
    )

    (outerDecls.toSeq, mAccessor)
  }

  private def projectionLemma(lemmaName: String, fun: Term, arg: Term, result: Term, proof: Proof) : LemmaDecl = {
    val statement = TermBinary(isabelle.ast.Eq, TermApp(fun, arg), result)
    LemmaDecl(lemmaName, statement, proof)
  }

}

