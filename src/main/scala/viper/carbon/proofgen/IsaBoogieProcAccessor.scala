package viper.carbon.proofgen

import isabelle.ast.{IsaTermUtil, IsaUtil, Term, TermIdent, TermTuple}
import viper.carbon.boogie.{LocalVar, Procedure}
import viper.carbon.verifier.Environment
import viper.silver.{ast => sil}

class IsaBoogieProcAccessor(
     val proc: Procedure,
     val procEnv: Environment,
     val procTheoryPath: String
   )
{

  val theoryName = org.apache.commons.io.FilenameUtils.getBaseName(procTheoryPath)

  /* compute the local variables. Must be the same order as computed by the Boogie pretty printer
     TODO: use common method to share the code

     Note that here we using that Boogie proof generation uses the variable identifier ordering:
     1) arguments, 2) local variables, 3) return variables
   */
  private val procVarMapping: Map[LocalVar, Int] = {
    val procLocalVars = proc.body.undeclLocalVars filter (v1 => (proc.ins ++ proc.outs).forall(v2 => v2.name != v1.name))

    ((proc.ins ++ proc.outs).map(d => d.l) ++ procLocalVars).zipWithIndex.toMap
  }

  //includes arguments, locally declared variables and out variables
  def getAllLocalVariables() : Set[LocalVar] = procVarMapping.keySet

  def getBoogieVarId(varVpr: sil.LocalVar) : Int = {
    getVarId(procEnv.get(varVpr))
  }

  def getVarId(varBpl: LocalVar) : Int = {
    procVarMapping.get(varBpl) match {
      case Some(idBpl) =>
        //TODO when fields and predicates are added, then will need to adjust this
        IsaBoogieProcAccessor.constantsIdMap.size + IsaBoogieProcAccessor.globalsOrderMap.size +
          idBpl
      case None => sys.error(s"Could not find related Boogie variable for Viper variable ${varBpl.name}")
    }
  }

  def getLookupThyThm(g: BoogieConstGlobal) = lookupThyThmFromId(getVarId(g))

  def getLookupThyThm(x: LocalVar) : String = lookupThyThmFromId(getVarId(x))

  private def lookupThyThmFromId(id: Int) : String = procDataDecl("lvar"+id)+"(2)"

  def getVarId(d: BoogieConstGlobal) : Int = {
    IsaBoogieProcAccessor.constantsIdMap.get(d).fold
       //None case: guaranteed to obtain value from map, since using sealed trait and both maps cover all possible values
      //TODO when fields and predicates are added, then will need to adjust this
      { IsaBoogieProcAccessor.constantsIdMap.size + IsaBoogieProcAccessor.globalsOrderMap.get(d).get  }
       //Some case
      { id => id }
  }

  private val globalDataTheoryName: String = "global_data"
  private def globalDataDecl(declName: String) = IsaUtil.qualifyName(globalDataTheoryName, declName)

  val funDecls : TermIdent = TermIdent(globalDataDecl("fdecls"))

  val funDeclsWf: TermIdent = TermIdent(globalDataDecl("funcs_wf"))

  private def procDataDecl(declName: String) = IsaUtil.qualifyName(theoryName, declName)

  val constDecls : TermIdent = TermIdent(globalDataDecl("constants_vdecls"))
  val globalDecls : TermIdent = TermIdent(globalDataDecl("globals_vdecls"))
  val paramsDecls : TermIdent = TermIdent(procDataDecl("params_vdecls"))
  val localsDecls : TermIdent = TermIdent(procDataDecl("locals_vdecls"))

  val varContext : Term =
    TermTuple(
      IsaTermUtil.appendList(constDecls, globalDecls),
      IsaTermUtil.appendList(paramsDecls, localsDecls)
    )

  val varContextWfThm: String = procDataDecl("var_context_wf")

  val procBodyAstDef: String = procDataDecl("proc_body")

}

case object IsaBoogieProcAccessor {

  private val constantsOrder : Seq[BoogieConstGlobal] =
    Seq(NullConst,
      AllocatedConst,
      ZeroMaskConst,
      ZeroPMaskConst,
      NoPermConst,
      FullPermConst,
      EmptyFrameConst)

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

case object HeapGlobalVar extends BoogieConstGlobal
case object MaskGlobalVar extends BoogieConstGlobal
