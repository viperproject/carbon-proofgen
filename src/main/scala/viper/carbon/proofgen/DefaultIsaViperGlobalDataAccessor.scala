package viper.carbon.proofgen
import isabelle.ast.{IsaUtil, Term, TermIdent}
import viper.silver.ast.Field
import viper.silver.{ast => sil}

case class DefaultIsaViperGlobalDataAccessor(override val theoryName: String,
                                             vprProgramIdent: String,
                                             override val origProgram: sil.Program,
                                             fieldsIdent: String,
                                             private val fieldToTerm : Map[sil.Field, Term],
                                             fieldRelIdent: String,
                                             override val fieldRelBoundedLemma: String,
                                             override val allFieldLookupLemmas: AllFieldLookupLemmas,
                                             methodsProgEqLemmaName: String,
                                             allMethodsAccessor: IsaViperAllMethodsAccessor,
                                             methodDataTableML: String
                                            ) extends IsaViperGlobalDataAccessor {

  private def qualifyName(name: String) =  IsaUtil.qualifyName(theoryName, name)

  override val vprProgram: TermIdent = TermIdent(qualifyName(vprProgramIdent))

  override def methodsProgEqLemma: String = qualifyName(methodsProgEqLemmaName)

  override val fields: TermIdent = TermIdent(qualifyName(fieldsIdent))
  override val fieldRel: TermIdent = TermIdent(qualifyName(fieldRelIdent))
  override def fieldIdent(f: Field): Term = fieldToTerm.get(f).get
  override def fieldLookupLemma(fieldName: String): String = ???
}
