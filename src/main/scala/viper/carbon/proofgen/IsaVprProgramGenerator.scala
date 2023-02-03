package viper.carbon.proofgen

import isabelle.ast.{AbbrevDecl, DefDecl, IsaTypeUtil, TermIdent, TermList, TermTuple, Theory, TupleType}
import viper.silver.{ast => sil}

import java.nio.file.Paths

object IsaVprProgramGenerator {


  //m must not be abstract
  def isaProgramRepr(m: sil.Method, theoryName: String, varTranslation: VarTranslation[sil.LocalVar]) : (Theory, IsaMethodAccessor) =
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
    val mAccessor = DefaultIsaMethodAccessor(methodTheoryPath = theoryName, methodBodyIdent = mBodyDecl.name, methodArgsIdent = mArgsDecl.name)

    (theory, mAccessor)
  }

}
