package viper.carbon.proofgen.end_to_end

import isabelle.ast.{IsaUtil, TermIdent}

case class FunInterpInstantiationData(funInterpVprBpl: String, funInterpVprBplWfLemma: String, funInterpBplWfLemma: String)

case class FieldRelInstantiationData(ranFieldRelLemma: String, fieldTrPropLemma: String)

case class DefaultIsaViperEndToEndGlobalData(
                        override val theoryName: String,
                        ctxtVprName: String,
                        programTotalProgEqLemmaName: String,
                        funInterpInstData : FunInterpInstantiationData,
                        fieldRelInstData: FieldRelInstantiationData,
                        axiomSatLemmaName: String) extends IsaViperEndToEndGlobalData
{

  def qualifyName(name: String) = IsaUtil.qualifyName(theoryName, name)

  override val ctxtVpr : TermIdent = TermIdent(qualifyName(ctxtVprName))

  override val programTotalProgEqLemma : String = qualifyName(programTotalProgEqLemmaName)

  override val funInterpVprBpl: TermIdent = TermIdent(qualifyName(funInterpInstData.funInterpVprBpl))

  override def funInterpVprBplWfLemma: String = qualifyName(funInterpInstData.funInterpVprBplWfLemma)

  override def funInterpBplWfLemma: String = qualifyName(funInterpInstData.funInterpBplWfLemma)

  override def fieldPropLemma: String = qualifyName(fieldRelInstData.fieldTrPropLemma)

  override def axiomSatLemma: String = qualifyName(axiomSatLemmaName)

}
