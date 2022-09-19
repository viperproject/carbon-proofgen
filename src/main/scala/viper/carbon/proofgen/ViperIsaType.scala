package viper.carbon.proofgen

import isabelle.ast._

import viper.silver.{ast => sil}


object ViperIsaType {

  val stmt : TypeIsa = DataType("stmt", Seq())

  def translate(ty : sil.Type) : Term = ty match {
    case sil.Bool => TermIdent("TBool")
    case sil.Int => TermIdent("TInt")
    case sil.Perm => TermIdent("TPerm")
    case sil.Ref => TermIdent("TRef")
    case _ => sys.error("do not support Viper type" + ty.toString())
  }

}
