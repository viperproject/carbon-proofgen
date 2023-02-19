package viper.carbon.proofgen
import isabelle.ast.{IsaUtil, Term, TermIdent}
import viper.silver.ast.Field
import viper.silver.{ast => sil}

case class DefaultIsaViperGlobalDataAccessor(
                                         override val theoryName: String,
                                         theoryDirPath: String,
                                         vprProgramIdent: String,
                                         fieldsIdent: String,
                                         private val fieldToTerm : Map[sil.Field, Term]) extends IsaViperGlobalDataAccessor {

  val theoryPath = theoryDirPath+"/"+theoryName

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryPath, name)

  override def vprProgram(): TermIdent = TermIdent(qualifyName(vprProgramIdent))

  override def fields(): TermIdent = TermIdent(qualifyName(fieldsIdent))
  override def fieldIdent(f: Field): Term = fieldToTerm.get(f).get
  override def fieldLookupLemma(fieldName: String): String = ???
}
