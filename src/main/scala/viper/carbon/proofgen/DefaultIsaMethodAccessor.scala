package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}
import viper.silver.ast.{Method, Program}

case class DefaultIsaMethodAccessor(override val theoryName: String,
                                    override val globalDataAccessor: IsaViperGlobalDataAccessor,
                                    methodBodyIdent: String,
                                    methodArgsIdent: String,
                                    methodDeclIdent: String,
                                    override val origProgram: Program,
                                    override val origMethod: Method) extends IsaViperMethodAccessor  {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override val methodBody : TermIdent = TermIdent(qualifyName(methodBodyIdent))

  override val methodArgs : TermIdent  = TermIdent(qualifyName(methodArgsIdent))

  override val methodDecl : TermIdent = TermIdent(qualifyName(methodDeclIdent))

  override def methodDeclProjectionLemmaName(methodDeclMember: MethodDeclMember): String =
    DefaultIsaMethodAccessor.methodDeclProjectionLemmaName(methodDeclMember)

}

case object DefaultIsaMethodAccessor {
  def methodDeclProjectionLemmaName(methodDeclMember: MethodDeclMember): String =
    s"mdecl_proj_${methodDeclMember.name}"

}
