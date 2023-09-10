package viper.carbon.proofgen.end_to_end

import isabelle.ast.{IsaUtil, TermIdent}

case class FunInterpInstantiationData(funInterpVprBpl: String, funInterpVprBplWfLemma: String, funInterpBplWfLemma: String)

case class FieldRelInstantiationData(ranFieldRelLemma: String, fieldTrPropLemma: String)

case class ConstantsData(lookupConstantsNoGlobalsLemma: String, constantsLookupWithGlobalsLemma: String)

case class DefaultIsaViperEndToEndGlobalData(
                        override val theoryName: String,
                        ctxtVprName: String,
                        programTotalProgEqLemmaName: String,
                        funInterpInstData : FunInterpInstantiationData,
                        constantsData: ConstantsData,
                        fieldRelInstData: FieldRelInstantiationData,
                        axiomSatLemmaName: String) extends IsaViperEndToEndGlobalData
{

  def qualifyName(name: String) = IsaUtil.qualifyName(theoryName, name)

  override val ctxtVpr : TermIdent = TermIdent(qualifyName(ctxtVprName))

  override val programTotalProgEqLemma : String = qualifyName(programTotalProgEqLemmaName)

  override val funInterpVprBpl: TermIdent = TermIdent(qualifyName(funInterpInstData.funInterpVprBpl))

  override def funInterpVprBplWfLemma: String = qualifyName(funInterpInstData.funInterpVprBplWfLemma)

  override def funInterpBplWfLemma: String = qualifyName(funInterpInstData.funInterpBplWfLemma)

  override def constantsLookupWithoutGlobalsLemma: String = qualifyName(constantsData.lookupConstantsNoGlobalsLemma)

  override def constantsLookupWithGlobalsLemma: String = qualifyName(constantsData.constantsLookupWithGlobalsLemma)

  override def ranFieldRelLemma: String =qualifyName(fieldRelInstData.ranFieldRelLemma)

  override def fieldPropLemma: String = qualifyName(fieldRelInstData.fieldTrPropLemma)

  override def axiomSatLemma: String = qualifyName(axiomSatLemmaName)
}
