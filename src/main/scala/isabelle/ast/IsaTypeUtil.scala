package isabelle.ast

object IsaTypeUtil {

  def listType(arg: TypeIsa) : TypeIsa = DataType("list", arg)
  def optionType(argType: TypeIsa) : TypeIsa = DataType("option", argType)

  val natType = DataType("nat", Seq())
  val boolType = DataType("bool", Seq())
  val stringType = DataType("string", Seq())

}
