package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}

case class DefaultIsaMethodAccessor(override val theoryName: String,
                                    methodBodyIdent: String,
                                    methodArgsIdent: String) extends IsaViperMethodAccessor  {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override val methodBody: TermIdent = TermIdent(qualifyName(methodBodyIdent))

  override val methodArgs: TermIdent  = TermIdent(qualifyName(methodArgsIdent))
}
