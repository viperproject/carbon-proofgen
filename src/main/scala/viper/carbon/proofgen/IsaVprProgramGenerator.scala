package viper.carbon.proofgen

import isabelle.ast.{AbbrevDecl, DefDecl, IsaTermUtil, IsaTypeUtil, IsaUtil, LemmaDecl, NatConst, OuterDecl, Proof, ProofUtil, StringConst, Term, TermIdent, TermList, TermTuple, Theory, TupleType}
import viper.silver.{ast => sil}

import scala.collection.mutable.ListBuffer

object IsaVprProgramGenerator {

  def globalData(p: sil.Program, boogieGlobalAccessor: IsaBoogieGlobalAccessor, theoryName: String, pathToTheoryDir: String) : (Theory, IsaViperGlobalDataAccessor) =
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
          fieldRelBoundedLemma = fieldRelationBoundedBy.name)
      )
    }


  //m must not be abstract
  def isaProgramRepr(m: sil.Method, theoryName: String, varTranslation: VarTranslation[sil.LocalVar], globalDataAccessor: IsaViperGlobalDataAccessor, vprProg: sil.Program) : (Theory, IsaViperMethodAccessor) =
  {
    if(m.body.isEmpty) {
      sys.error("invoked isaProgramRepr with abstrasct method")
    }

    val mBodyTerm = ViperToIsa.translateStmt(m.body.get)(varTranslation)
    val mBodyDecl = DefDecl("method_body", Some(ViperIsaType.stmt), (Seq(), mBodyTerm))

    //for now keep arguments and return variables in the same list
    val mArgsAndReturns = (m.formalArgs ++ m.formalReturns).map(varDecl =>  {
      varTranslation.translateVariableId(varDecl.localVar) match {
        case Some(id) =>
          println(s"${varDecl.localVar.name},${id.toString}")
          TermTuple(Seq(TermIdent(id.toString), ViperIsaType.translate(varDecl.typ)))
        case None => sys.error(s"could not translate local variable ${varDecl.localVar.toString()}")
      }
    })

    val mArgsDecl = AbbrevDecl(
      "method_args_and_returns",
      Some(IsaTypeUtil.listType( TupleType(Seq(ViperIsaType.varNameType, ViperIsaType.viperTyType)) )),
      (Seq(), TermList(mArgsAndReturns))
    )

    val theory = Theory(theoryName, Seq("Viper.ViperLang"), Seq(mBodyDecl, mArgsDecl))
    val mAccessor = DefaultIsaMethodAccessor(
      theoryName = theoryName,
      globalDataAccessor = globalDataAccessor,
      methodBodyIdent = mBodyDecl.name,
      methodArgsIdent = mArgsDecl.name,
      origProgram = vprProg)

    (theory, mAccessor)
  }

}
