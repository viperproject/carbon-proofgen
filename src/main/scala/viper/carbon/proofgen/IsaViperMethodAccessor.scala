package viper.carbon.proofgen

import isabelle.ast.{IsaTermUtil, Term, TermApp, TermIdent}
import viper.silver.{ast => sil}

trait IsaViperGlobalDataAccessor extends IsaViperFieldsAccessor {

  def theoryName : String
  def vprProgram : TermIdent

  def origProgram : sil.Program

  def methodsProgEqLemma : String

  def allMethodsAccessor: IsaViperAllMethodsAccessor

  /**
    * Stores the data from [[allMethodsAccessor]] in an ML symbol table
    * @return
    */
  def methodDataTableML : String

}

trait IsaViperFieldsAccessor {
  def fields : TermIdent
  def fieldRel : TermIdent
  def fieldRelBoundedLemma : String
  def fieldIdent(f: sil.Field) : Term
  def fieldLookupLemma(fieldName: String) : String
}

trait IsaViperMethodAccessor {

  def theoryName: String
  def methodBody : TermIdent
  def methodArgs : TermIdent
  def methodRets : TermIdent
  def methodDecl : TermIdent
  def methodDeclProjectionLemmaName(methodDeclMember: MethodDeclMember) : String
  def origProgram : sil.Program
  def origMethod : sil.Method

}

trait IsaViperAllMethodsAccessor {
  def methodLookupFun : TermIdent
  def lookupLemmaName(methodName: String) : String
  def methodAccessor(methodName: String) : IsaViperMethodAccessor
}

sealed trait MethodDeclMember {
  def name: String
}
case object IsaMethodArgTypes extends MethodDeclMember
{
  override def name: String = "margs"
}
case object IsaMethodRetTypes extends MethodDeclMember
{
  override def name: String = "mrets"
}
case object IsaMethodPrecondition extends MethodDeclMember
{
  override def name: String = "mpre"
}
case object IsaMethodPostcondition extends MethodDeclMember
{
  override def name: String = "mpost"
}
case object IsaMethodBody extends MethodDeclMember
{
  override def name: String = "mbody"
}

object IsaMethodDecl {

  def allMethodDeclMembers : Seq[MethodDeclMember] =
    Seq(IsaMethodArgTypes, IsaMethodRetTypes, IsaMethodPrecondition, IsaMethodPostcondition, IsaMethodBody)

  def makeMethodDeclRecord(argTypes: Term, retTypes: Term, precondition: Term, postcondition: Term, methodBody: Term) : Term= {
    IsaTermUtil.makeRecord("method_decl", Seq(argTypes, retTypes, precondition, postcondition, methodBody))
  }

  def methodDeclProjectionFunction(methodDeclMember: MethodDeclMember) : Term = {
    methodDeclMember match {
      case IsaMethodArgTypes => TermIdent("method_decl.args")
      case IsaMethodRetTypes => TermIdent("method_decl.rets")
      case IsaMethodPrecondition => TermIdent("method_decl.pre")
      case IsaMethodPostcondition => TermIdent("method_decl.post")
      case IsaMethodBody => TermIdent("method_decl.body")
    }
  }

  def methodDeclProjection(methodDecl: Term, methodDeclMember: MethodDeclMember) : Term =
    TermApp(methodDeclProjectionFunction(methodDeclMember), methodDecl)

}
