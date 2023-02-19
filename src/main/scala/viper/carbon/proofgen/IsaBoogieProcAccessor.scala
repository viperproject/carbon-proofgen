package viper.carbon.proofgen

import isabelle.ast.{IsaTermUtil, IsaUtil, Term, TermIdent, TermTuple}
import viper.carbon.boogie.{LocalVar, Procedure}
import viper.carbon.verifier.Environment
import viper.silver.{ast => sil}

class IsaBoogieProcAccessor(
                             val proc: Procedure,
                             val procEnv: Environment,
                             val globalDataAccessor: IsaBoogieGlobalAccessor,
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
          globalDataAccessor.numGlobalsAndConstants+idBpl
      case None => sys.error(s"Could not find related Boogie variable for Viper variable ${varBpl.name}")
    }
  }

  def getLookupThyThm(g: BoogieConstGlobal) : String = lookupThyThmFromId(globalDataAccessor.getVarId(g))
  def getLookupThyThm(x: LocalVar) : String = lookupThyThmFromId(getVarId(x))

  private def lookupThyThmFromId(id: Int) : String = procDataDecl("lvar"+id)+"(2)"

  private def procDataDecl(declName: String) = IsaUtil.qualifyName(theoryName, declName)

  val paramsDecls : TermIdent = TermIdent(procDataDecl("params_vdecls"))
  val localsDecls : TermIdent = TermIdent(procDataDecl("locals_vdecls"))

  val varContext : Term =
    TermTuple(
      IsaTermUtil.appendList(globalDataAccessor.constDecls, globalDataAccessor.globalDecls),
      IsaTermUtil.appendList(paramsDecls, localsDecls)
    )

  val varContextWfThm: String = procDataDecl("var_context_wf")

  val procBodyAstDef: String = procDataDecl("proc_body")

}

