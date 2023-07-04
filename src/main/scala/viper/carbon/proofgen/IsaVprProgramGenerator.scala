package viper.carbon.proofgen

import isabelle.ast.{AbbrevDecl, DefDecl, IsaTermUtil, IsaTypeUtil, IsaUtil, LemmaDecl, NatConst, OuterDecl, Proof, ProofUtil, StringConst, Term, TermApp, TermBinary, TermIdent, TermList, TermTuple, Theory, TupleType}
import viper.silver.{ast => sil}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object IsaVprProgramGenerator {

  def globalData(p: sil.Program, boogieGlobalAccessor: IsaBoogieGlobalAccessor, theoryName: String, pathToTheoryDir: String) : (Theory, IsaViperGlobalDataAccessor, Seq[Theory]) =
    {
      val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty

      val fieldToTerm : Map[sil.Field, Term] = p.fields.map(f => (f, StringConst(f.name))).toMap

      val fieldsList = TermList(p.fields.map(f => TermTuple(fieldToTerm.get(f).get, ViperIsaType.translate(f.typ))))

      val fieldsListDef = DefDecl("fields", None, (Seq(), fieldsList))
      outerDecls += fieldsListDef

      val fieldRelationList = {
        TermList(
          p.fields.map(f => {
            val vprFieldConstTerm = fieldToTerm.get(f).get
            val bplFieldConstId = boogieGlobalAccessor.getVarId(FieldConst(f))
            TermTuple(vprFieldConstTerm, NatConst(bplFieldConstId))
          }
          )
        )
      }

      val fieldRelationListDef = DefDecl(
        "field_rel",
        Some(IsaTypeUtil.listType(TupleType(IsaTypeUtil.stringType, BoogieIsaType.varNameType))),
        (Seq(), fieldRelationList)
      )

      outerDecls += fieldRelationListDef

      val fieldRelationBoundedBy = LemmaDecl(
        "field_rel_bound",
        ViperBoogieIsaUtil.allVarsInListBoundedBy(
          TermIdent(fieldRelationListDef.name),
          ViperBoogieIsaUtil.maxInRangeOfList(fieldRelationList)
        ),
        Proof(Seq(ProofUtil.byTac(ProofUtil.simpTac(IsaUtil.definitionLemmaFromName(fieldRelationListDef.name)))))
      )

      outerDecls += fieldRelationBoundedBy

      val (methodTheory, methodAccessors) = methodsRepr("method_decls", p)

      val programDef = DefDecl("vpr_prog",
        Some(ViperIsaType.vprProgramType),
        (Seq(), IsaTermUtil.makeRecord(ViperIsaType.vprProgramTypeName, Seq(
          TermIdent("f_None"),//TODO: methods
          TermIdent("f_None"),//TODO: predicates
          TermIdent("f_None"),//TODO: functions
          IsaTermUtil.mapOf(TermIdent(fieldsListDef.name)), //functions
          NatConst(0)) //domains
        ))
      )

      outerDecls += programDef

      (
        Theory(theoryName, Seq("Viper.ViperLang", "TotalViper.TotalUtil"), outerDecls.toSeq),
        DefaultIsaViperGlobalDataAccessor(
          theoryName = theoryName,
          vprProgramIdent = programDef.name,
          fieldsIdent = fieldsListDef.name,
          fieldToTerm = fieldToTerm,
          fieldRelIdent = fieldRelationListDef.name,
          fieldRelBoundedLemma = fieldRelationBoundedBy.name,
          methodAccessors = methodAccessors
        ),
        Seq(methodTheory)
      )
    }

  private def methodsRepr(theoryName: String, p: sil.Program) : (Theory, Map[String, IsaViperMethodAccessor]) = {
    val outerDecls : ListBuffer[OuterDecl] = ListBuffer.empty
    val map: mutable.Map[String, IsaViperMethodAccessor] = mutable.Map.empty

    for(m <- p.methods) {
      //TODO: put variable translation in a common place
      val varTranslation = DeBruijnTranslation.freshTranslation((m.formalArgs ++ m.formalReturns) map (varDecl => varDecl.localVar))

      val (mOuterDecls, mAccessor) = methodProgramRepr(m, theoryName, varTranslation, p)
      outerDecls.addAll(mOuterDecls)
      map.put(m.name, mAccessor)
    }

    val theory = Theory(theoryName, Seq("Viper.ViperLang"), outerDecls.toSeq)
    (theory, map.toMap)
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

    //for now keep arguments and return variables in the same list
    val mArgsAndReturns = (m.formalArgs ++ m.formalReturns).map(varDecl =>  {
      varTranslation.translateVariableId(varDecl.localVar) match {
        case Some(id) =>
          println(s"${varDecl.localVar.name},${id.toString}")
          (TermIdent(id.toString), ViperIsaType.translate(varDecl.typ))
        case None => sys.error(s"could not translate local variable ${varDecl.localVar.toString()}")
      }
    })

    val mArgsAndReturnsTypesTerm = TermList(mArgsAndReturns.map( { case (_, typ) => typ }))
    val mArgsAndReturnsTerm = TermList(mArgsAndReturns.map( { case (id, typ) => TermTuple(id,typ)}))

    val mArgsDecl = AbbrevDecl(
      outerDeclName("args_and_returns"),
      Some(IsaTypeUtil.listType( TupleType(Seq(ViperIsaType.varNameType, ViperIsaType.viperTyType)) )),
      (Seq(), mArgsAndReturnsTerm)
    )

    outerDecls += mArgsDecl
    methodDeclMemberToResultAndDef.put(IsaMethodArgTypes, (mArgsAndReturnsTypesTerm,None))

    methodDeclMemberToResultAndDef.put(IsaMethodRetTypes, (TermList(Seq()), None)) //TODO: split arguments and returns

    val mPreTerm = ViperToIsa.translateAssertion(IsaMethodSpecificationHelper.conjoinSpecAssertions(m.pres))(varTranslation)
    val mPreDecl = DefDecl(outerDeclName("pre"), None, (Seq(), mPreTerm))

    outerDecls += mPreDecl
    methodDeclMemberToResultAndDef.put(IsaMethodPrecondition, (mPreTerm, Some(IsaUtil.definitionLemmaFromName(mPreDecl.name)))) //directly go to term instead of definition

    val mPostTerm = ViperToIsa.translateAssertion(IsaMethodSpecificationHelper.conjoinSpecAssertions(m.posts))(varTranslation)
    val mPostDecl = DefDecl(outerDeclName("post"), None, (Seq(), mPostTerm))

    outerDecls += mPostDecl
    methodDeclMemberToResultAndDef.put(IsaMethodPostcondition, (mPostTerm, Some(IsaUtil.definitionLemmaFromName(mPostDecl.name))))

    val methodDeclTerm = IsaMethodDecl.makeMethodDeclRecord(
      argTypes = mArgsAndReturnsTypesTerm,
      retTypes = TermList(Seq()), //TODO: split args and returns
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

