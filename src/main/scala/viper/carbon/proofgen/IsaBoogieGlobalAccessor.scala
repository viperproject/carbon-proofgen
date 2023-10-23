package viper.carbon.proofgen

import isabelle.ast.{IsaUtil, TermIdent}
import viper.silver.{ast => sil}

case class IsaBoogieGlobalAccessor(theoryName: String, fields: Seq[sil.Field]) {

  private val fieldToIdx : Map[sil.Field, Int] = fields.zipWithIndex.toMap

  private val globalDataTheoryName: String = "global_data"
  private def globalDataDecl(declName: String) = IsaUtil.qualifyName(globalDataTheoryName, declName)

  val funDecls : TermIdent = TermIdent(globalDataDecl("fdecls"))

  val funDeclsWf: TermIdent = TermIdent(globalDataDecl("funcs_wf"))

  val constDecls : TermIdent = TermIdent(globalDataDecl("constants_vdecls"))
  val uniqueConsts : TermIdent = TermIdent(globalDataDecl("unique_consts"))
  val globalDecls : TermIdent = TermIdent(globalDataDecl("globals_vdecls"))
  val axiomDecls : TermIdent = TermIdent(globalDataDecl("axioms"))

  def getVarId(g: BoogieConstGlobal) : Int = {
    IsaBoogieGlobalAccessor.constantsIdMap.get(g).fold
    //None case: guaranteed to obtain value from map, since using sealed trait and both maps cover all possible values
    //TODO when predicates are added, then will need to adjust this
    {
      val base : Int = IsaBoogieGlobalAccessor.constantsIdMap.size

      val idx : Int =
        g match {
          case FieldConst(field) if fieldToIdx.contains(field) =>
            fieldToIdx.get(field).get
          case _ => fieldToIdx.size+IsaBoogieGlobalAccessor.globalsOrderMap.get(g).get
        }

      base + idx
    }
    //Some case
    { id => id }
  }

  def getGlobalMapOfThm(g: BoogieConstGlobal) : String = IsaUtil.qualifyName(theoryName, "mvar"+getVarId(g))
  def getConstantWithoutGlobalsMapOfThm(g: BoogieConstGlobal) : String = IsaUtil.qualifyName(theoryName, "mconst"+getVarId(g))

  val numGlobalsAndConstants : Int = {
    IsaBoogieGlobalAccessor.constantsIdMap.size+
      fields.size +
      IsaBoogieGlobalAccessor.globalsOrderMap.size
  }

}

case object IsaBoogieGlobalAccessor {

  private val constantsOrder : Seq[BoogieConstGlobal] =
    Seq(NullConst,
      //AllocatedConst, (relevant if do not use --disableAllocEncoding option)
      ZeroMaskConst,
      //ZeroPMaskConst,
      NoPermConst,
      FullPermConst)
      //EmptyFrameConst)

  private val constantsIdMap : Map[BoogieConstGlobal, Int] =
    constantsOrder.zipWithIndex.toMap

  private val globalsOrder :  Seq[BoogieConstGlobal] =
    Seq(HeapGlobalVar, MaskGlobalVar)

  private val globalsOrderMap : Map[BoogieConstGlobal, Int] =
    globalsOrder.zipWithIndex.toMap

}

sealed trait BoogieConstGlobal

case object NullConst extends BoogieConstGlobal
case object AllocatedConst extends BoogieConstGlobal
case object ZeroMaskConst extends BoogieConstGlobal
case object ZeroPMaskConst extends BoogieConstGlobal
case object NoPermConst extends BoogieConstGlobal
case object FullPermConst extends BoogieConstGlobal
case object EmptyFrameConst extends BoogieConstGlobal
case class FieldConst(field: sil.Field) extends BoogieConstGlobal

case object HeapGlobalVar extends BoogieConstGlobal
case object MaskGlobalVar extends BoogieConstGlobal
