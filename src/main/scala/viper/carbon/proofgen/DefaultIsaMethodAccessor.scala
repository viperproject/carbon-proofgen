package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultIsaMethodAccessor(override val methodTheoryPath: String,
                                    methodBodyIdent: String,
                                    methodArgsIdent: String) extends IsaMethodAccessor  {

  val theoryName = org.apache.commons.io.FilenameUtils.getBaseName(methodTheoryPath)
  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override val methodBody: TermIdent = TermIdent(qualifyName(methodBodyIdent))

  override val methodArgs: TermIdent  = TermIdent(qualifyName(methodArgsIdent))
}
