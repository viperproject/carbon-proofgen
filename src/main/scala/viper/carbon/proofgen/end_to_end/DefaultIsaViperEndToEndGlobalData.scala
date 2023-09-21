package viper.carbon.proofgen.end_to_end

import isabelle.ast.{IsaUtil, TermIdent, TypeIsa}

case class FunInterpInstantiationData(funInterpVprBpl: String, funInterpVprBplWfLemma: String, funInterpBplWfLemma: String)

case class FieldRelInstantiationData(ranFieldRelLemma: String, injFieldRelLemma: String, fieldTrPropNoGlobalsLemma: String, fieldTrPropWithGlobalsLemma: String)

case class ConstantsData(lookupConstantsNoGlobalsLemma: String, constantsLookupWithGlobalsLemma: String)

case class DefaultIsaViperEndToEndGlobalData(
                        override val theoryName: String,
                        ctxtVprName: String,
                        override val abstractValueType: TypeIsa,
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

  override def ranFieldRelLemma: String = qualifyName(fieldRelInstData.ranFieldRelLemma)

  override def injFieldRelLemma: String = qualifyName(fieldRelInstData.injFieldRelLemma)

  override def fieldPropWithoutGlobalsLemma: String = qualifyName(fieldRelInstData.fieldTrPropNoGlobalsLemma)

  override def fieldPropWithGlobalsLemma: String = qualifyName(fieldRelInstData.fieldTrPropWithGlobalsLemma)

  override def axiomSatLemma: String = qualifyName(axiomSatLemmaName)

}
