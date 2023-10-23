package viper.carbon.proofgen.end_to_end

import isabelle.ast.{ContextElem, IsaTermUtil, IsaThmUtil, IsaUtil, Lambda, LemmaDecl, OuterDecl, Proof, ProofUtil, StringConst, Term, TermApp, TermBinary, TermIdent, TermList, TermQuantifier, TermTuple, TermWithExplicitType, Theory}
import viper.carbon.proofgen.{IsaViperGlobalDataAccessor, ViperBoogieRelationIsa, ViperIsaTerm, ViperMethodCorrectness, ViperProgramRecord, ViperTotalContext}
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

    val outerDecls = generateAllMethodsCorrect("all_methods_correct", globalEndToEndData, endToEndProofData, viperProgAccessor)

    Theory(theoryName = theoryName, importTheories = methodEndToEndProofImports, decls = outerDecls)
  }

  private def generateAllMethodsCorrect(
                    lemmaName: String,
                    globalEndToEndData: IsaViperEndToEndGlobalData,
                    endToEndProofData2: Map[sil.Method, MethodEndToEndProofData],
                    viperProgAccessor: IsaViperGlobalDataAccessor) : Seq[OuterDecl] = {

    //the order of methods should be traversed the same way in all cases such that assumptions and conclusion + method lookup definition are consistent

    val endToEndProofDataSeq = viperProgAccessor.allMethodsAccessor.methodOrder.map(method => endToEndProofData2.get(method).get)

    if(endToEndProofDataSeq.size != viperProgAccessor.allMethodsAccessor.methodOrder.size) {
      throw new IllegalArgumentException()
    }

    val boogieProcedureCorrectnessAssms = endToEndProofDataSeq.map(m => m.boogieProcCorrectAssumption)

    val vprContextType = ViperTotalContext.totalContextRecordType(globalEndToEndData.abstractValueType)
    val vprContextWithExplicitType = TermWithExplicitType(globalEndToEndData.ctxtVpr, vprContextType)

    val methodNameAndDeclTupleList = viperProgAccessor.allMethodsAccessor.methodOrder.map( method =>
      {
        val methodAccessor = viperProgAccessor.allMethodsAccessor.methodAccessor(method.name)
        TermTuple(Seq(StringConst(method.name), methodAccessor.methodDecl))
      }
    )

    val conclusionAux : Term =
      {
        val methodCorrectPartialTerm = TermApp(TermIdent(ViperMethodCorrectness.vprMethodCorrectPartialDefName),
          Seq(vprContextWithExplicitType,
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
        //only execute if tac succeeds, since if there is only one method, then intro conjI fails
        ProofUtil.ifTacSucceedsTac(ProofUtil.introTac("conjI")),
      ) ++
        endToEndProofDataSeq.zipWithIndex.map({
          case (proofData, idx) =>
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
        ViperProgramRecord.methods(viperProgAccessor.vprProgram),
        methodTerm
      ),
      IsaTermUtil.some(methodDeclTerm)
    )

    val auxPartialLemma = LemmaDecl(
      lemmaName+"_aux",
      ContextElem.onlyAssumptionsNoLabels(
        methodLookupAssumption +:
        boogieProcedureCorrectnessAssms
      ),
      ViperMethodCorrectness.correctPartial(
        vprContextWithExplicitType,
        ViperBoogieRelationIsa.trivialStateConsistency,
        methodDeclTerm),
      Proof(Seq(
        ProofUtil.byTac(
          ProofUtil.ruleTac(
            ProofUtil.multiAttributes(
              IsaThmUtil.mapOfListAll, Seq(
                ("OF", Seq(ProofUtil.simplified("assms(1)", Seq(viperProgAccessor.methodsProgEqLemma, IsaUtil.definitionLemmaFromName(viperProgAccessor.allMethodsAccessor.methodLookupFun.toString))),
                  auxListAllPartialLemma.name)
                ),
                ("OF", Seq("assms(2-)"))
              )
            )
          )
        )
      ))
    )

    val methodLookupAssumptionWithVprContext = TermBinary.eq(
      TermApp(
        ViperProgramRecord.methods(ViperTotalContext.programTotal(vprContextWithExplicitType)),
        methodTerm
      ),
      IsaTermUtil.some(methodDeclTerm)
    )

    val endToEndLemma = LemmaDecl(
      lemmaName,
      ContextElem.onlyAssumptionsNoLabels(
        methodLookupAssumptionWithVprContext +:
          boogieProcedureCorrectnessAssms
      ),
      ViperMethodCorrectness.correct(
        vprContextWithExplicitType,
        ViperBoogieRelationIsa.trivialStateConsistency,
        methodDeclTerm),
      Proof(
        ProofUtil.mapApplyTac(Seq(
          ProofUtil.ruleTac(ProofUtil.OF(ViperMethodCorrectness.vprMethodCorrectFromPartialThm, "assms(1)")),
          ProofUtil.ruleTac(ProofUtil.OF(auxPartialLemma.name, Seq("_", "assms(2-)"))),
          ProofUtil.simpTacOnly(globalEndToEndData.programTotalProgEqLemma)
        ))
        :+ ProofUtil.doneTac
      )
    )

    Seq(auxListAllPartialLemma, auxPartialLemma, endToEndLemma)
  }

}
