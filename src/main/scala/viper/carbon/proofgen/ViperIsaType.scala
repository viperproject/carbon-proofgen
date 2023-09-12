package viper.carbon.proofgen

import isabelle.ast._

import viper.silver.{ast => sil}


object ViperIsaType {

  val stmt : TypeIsa = DataType("stmt", Seq())

  val varNameType: TypeIsa = IsaTypeUtil.natType
  val viperTyType: TypeIsa = DataType("vtyp", Seq())

  val vprProgramTypeName : String = "program"
  val vprProgramType: TypeIsa = DataType(vprProgramTypeName, Seq())

  def totalContext(absType: TypeIsa) = DataType("total_context", absType)

  def translate(ty : sil.Type) : Term = ty match {
    //use explicit accesses to avoid clashes with the Boogie formalization
    case sil.Bool => TermIdent("ViperLang.TBool")
    case sil.Int => TermIdent("ViperLang.TInt")
    case sil.Perm => TermIdent("ViperLang.TPerm")
    case sil.Ref => TermIdent("ViperLang.TRef")
    case _ => sys.error("do not support Viper type" + ty.toString())
  }

}
