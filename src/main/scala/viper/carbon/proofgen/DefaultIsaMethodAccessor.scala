package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}
import viper.silver.ast.Program

case class DefaultIsaMethodAccessor(override val theoryName: String,
                                    override val globalDataAccessor: IsaViperGlobalDataAccessor,
                                    methodBodyIdent: String,
                                    methodArgsIdent: String,
                                    override val origProgram: Program) extends IsaViperMethodAccessor  {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override val methodBody: TermIdent = TermIdent(qualifyName(methodBodyIdent))

  override val methodArgs: TermIdent  = TermIdent(qualifyName(methodArgsIdent))

}
