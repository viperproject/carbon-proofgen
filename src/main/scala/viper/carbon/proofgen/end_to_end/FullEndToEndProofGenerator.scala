package viper.carbon.proofgen.end_to_end

import isabelle.ast.{ContextElem, IsaTermUtil, Lambda, LemmaDecl, OuterDecl, Proof, ProofUtil, StringConst, Term, TermApp, TermBinary, TermIdent, TermList, TermQuantifier, TermTuple, TermWithExplicitType, Theory}
import viper.carbon.proofgen.{IsaViperGlobalDataAccessor, ViperBoogieRelationIsa, ViperIsaTerm, ViperMethodCorrectness, ViperTotalContext}
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

    val outerDecls = generateAllMethodsCorrect("all_methods_correct", globalEndToEndData, endToEndProofDataSeq, viperProgAccessor)

    Theory(theoryName = theoryName, importTheories = methodEndToEndProofImports, decls = outerDecls)
  }

  private def generateAllMethodsCorrect(
                    lemmaName: String,
                    globalEndToEndData: IsaViperEndToEndGlobalData,
                    endToEndProofData: Seq[(sil.Method, MethodEndToEndProofData)],
                    viperProgAccessor: IsaViperGlobalDataAccessor) : Seq[OuterDecl] = {

    val boogieProcedureCorrectnessAssms = endToEndProofData.map(m => m._2.boogieProcCorrectAssumption)

    val vprContextType = ViperTotalContext.totalContextRecordType(globalEndToEndData.abstractValueType)

    val methodNameAndDeclTupleList = endToEndProofData.map( {
        case (method, _) =>
          val methodAccessor = viperProgAccessor.allMethodsAccessor.methodAccessor(method.name)
          TermTuple(Seq(StringConst(method.name), methodAccessor.methodDecl))
      }
    )

    val conclusionAux : Term =
      {
        val methodCorrectPartialTerm = TermApp(TermIdent(ViperMethodCorrectness.vprMethodCorrectPartialDefName),
          Seq(TermWithExplicitType(globalEndToEndData.ctxtVpr, vprContextType),
              ViperBoogieRelationIsa.trivialStateConsistency
          )
        )

        IsaTermUtil.listAll(
          pred = IsaTermUtil.compose(methodCorrectPartialTerm, IsaTermUtil.sndId),
          list = TermList(methodNameAndDeclTupleList)
        )
      }

    val proofMethodsAux = ProofUtil.mapApplyTac(
      Seq(
        ProofUtil.simp,
        ProofUtil.introTac("conjI"),
      ) ++
        endToEndProofData.zipWithIndex.map({
          case ((_,proofData), idx) =>
            ProofUtil.ruleTac(ProofUtil.OF(proofData.endToEndLemma, s"assms(${idx+1})"))
        })
    )

    val auxListAllPartialLemma = LemmaDecl(
      lemmaName+"_list_all_aux",
      ContextElem.onlyAssumptionsNoLabels(
        boogieProcedureCorrectnessAssms
      ),
      conclusionAux,
      Proof(proofMethodsAux :+ ProofUtil.doneTac)
    )

    val methodTerm = TermIdent("m")
    val methodDeclTerm = TermIdent("mdecl")

    val methodLookupAssumption = TermBinary.eq(
      TermApp(
        ViperIsaTerm.methodsOfProgramProjection(viperProgAccessor.vprProgram),
        methodTerm
      ),
      IsaTermUtil.some(methodDeclTerm)
    )

    val mainLemmaContextElem = ContextElem.onlyAssumptionsNoLabels(
      methodLookupAssumption +:
        boogieProcedureCorrectnessAssms
    )

    val auxPartialLemma = LemmaDecl(
      lemmaName+"_aux",
      mainLemmaContextElem,
      ViperMethodCorrectness.correctPartial(
        TermWithExplicitType(globalEndToEndData.ctxtVpr, vprContextType),
        ViperBoogieRelationIsa.trivialStateConsistency,
        methodDeclTerm),
      Proof(Seq("oops"))
    )

    val endToEndLemma = LemmaDecl(
      lemmaName,
      mainLemmaContextElem,
      ViperMethodCorrectness.correct(
        TermWithExplicitType(globalEndToEndData.ctxtVpr, vprContextType),
        ViperBoogieRelationIsa.trivialStateConsistency,
        methodDeclTerm),
      Proof(Seq("oops"))
    )

    Seq(auxListAllPartialLemma, auxPartialLemma, endToEndLemma)
  }

}
