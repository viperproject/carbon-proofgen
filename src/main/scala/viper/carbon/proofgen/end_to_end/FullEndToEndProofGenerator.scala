package viper.carbon.proofgen.end_to_end

import isabelle.ast.{ContextElem, IsaTermUtil, Lambda, LemmaDecl, OuterDecl, Proof, Term, TermApp, TermIdent, TermList, TermQuantifier, TermTuple, TermWithExplicitType, Theory}
import viper.carbon.proofgen.{IsaViperGlobalDataAccessor, ViperBoogieRelationIsa, ViperMethodCorrectness, ViperTotalContext}
import viper.silver.{ast => sil}

import scala.collection.mutable.ListBuffer

object FullEndToEndProofGenerator {

  /**
    *
    * @param theoryName
    * @param endToEndProofData provides the data for the method end-to-end proofs. All end-to-end proofs in the map must have the same value
    *                          for the abstract value type specified in [[MethodEndToEndProofData.boogieProcCorrectAssumption]]
    * @param methodEndToEndProofImports theory files that contain the method end-to-end proofs
    * @param viperProgAccessor
    * @return
    */
  def generateFullEndToEndProof(theoryName: String,
                                globalEndToEndData: IsaViperEndToEndGlobalData,
                                endToEndProofData: Map[sil.Method, MethodEndToEndProofData],
                                methodEndToEndProofImports: Seq[String],
                                viperProgAccessor: IsaViperGlobalDataAccessor) : Theory = {
    val endToEndProofDataSeq = endToEndProofData.toSeq //define an iteration order on the methods, since will need to iterate multiple times in the same order

    val outerDecls = ListBuffer[OuterDecl]()

    val allMethodsCorrectPartialLemmaName = "all_methods_correct_partial"
    val allMethodsCorrectLemmaName = "all_methods_correct"

    outerDecls.addAll(generateAllMethodsPartialCorrect(allMethodsCorrectPartialLemmaName, globalEndToEndData, endToEndProofDataSeq, viperProgAccessor))
    outerDecls.addAll(generateAllMethodsCorrect(allMethodsCorrectLemmaName, endToEndProofDataSeq, viperProgAccessor, allMethodsCorrectPartialLemmaName))

    Theory(theoryName = theoryName, importTheories = methodEndToEndProofImports, decls = outerDecls.toSeq)
  }

  private def generateAllMethodsPartialCorrect(
                    lemmaName: String,
                    globalEndToEndData: IsaViperEndToEndGlobalData,
                    endToEndProofData: Seq[(sil.Method, MethodEndToEndProofData)],
                    viperProgAccessor: IsaViperGlobalDataAccessor) : Seq[OuterDecl] = {

    val boogieProcedureCorrectnessAssms : ContextElem =
      ContextElem.onlyAssumptionsNoLabels(
        endToEndProofData.map(m => m._2.boogieProcCorrectAssumption)
      )

    val methodNameAndDeclTupleList = endToEndProofData.map( {
        case (method, _) =>
          val methodAccessor = viperProgAccessor.allMethodsAccessor.methodAccessor(method.name)
          TermTuple(Seq(TermIdent(method.name), methodAccessor.methodDecl))
      }
    )

    val conclusion : Term =
      {
        val methodCorrectPartialTerm = TermApp(TermIdent(ViperMethodCorrectness.vprMethodCorrectPartialDefName),
          Seq(TermWithExplicitType(globalEndToEndData.ctxtVpr, ViperTotalContext.totalContextRecordType(globalEndToEndData.abstractValueType)),
              ViperBoogieRelationIsa.trivialStateConsistency
          )
        )

        IsaTermUtil.listAll(
          pred = IsaTermUtil.compose(methodCorrectPartialTerm, IsaTermUtil.sndId),
          list = TermList(methodNameAndDeclTupleList)
        )
      }

    val lemmaAux = LemmaDecl(
      lemmaName+"_aux",
      boogieProcedureCorrectnessAssms,
      conclusion,
      Proof(Seq("oops"))
    )

    Seq(lemmaAux)
  }

  private def generateAllMethodsCorrect(lemmaName: String, endToEndProofData: Seq[(sil.Method, MethodEndToEndProofData)],
                                        viperProgAccessor: IsaViperGlobalDataAccessor, allMethodsCorrectPartialLemmaName: String) : Seq[OuterDecl] = {
    Seq()
  }

}
