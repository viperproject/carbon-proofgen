package viper.carbon.proofgen

import isabelle.ast.{DataType, IsaTypeUtil, TypeIsa}

object BoogieIsaType {

  val varNameType: TypeIsa = IsaTypeUtil.natType

  val astType: TypeIsa = DataType("ast", Seq())

}
