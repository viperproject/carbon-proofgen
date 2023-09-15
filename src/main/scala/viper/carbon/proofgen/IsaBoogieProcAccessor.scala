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
     1) arguments, 2) return, variables 3) local variables,
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

  def getGlobalLookupDeclThm(g: BoogieConstGlobal) : String = lookupTyThmFromId(globalDataAccessor.getVarId(g), true)
  def getGlobalLookupTyThm(g: BoogieConstGlobal) : String = lookupTyThmFromId(globalDataAccessor.getVarId(g), false)
  def getLocalLookupDeclThm(x: LocalVar) : String = lookupTyThmFromId(getVarId(x), true)
  def getLocalLookupTyThm(x: LocalVar) : String = lookupTyThmFromId(getVarId(x), false)
  def getLocalMapOfThm(x: LocalVar) : String = mapOfThmFromId(getVarId(x))

  private def lookupTyThmFromId(id: Int, withFullDecl: Boolean) : String =
    procDataDecl("lvar"+id) + (if(withFullDecl) { "(1)" } else { "(2)" })

  def mapOfThmFromId(id: Int) : String = procDataDecl("mvar"+id)

  private def procDataDecl(declName: String) = IsaUtil.qualifyName(theoryName, declName)

  val globalsLocalsDisjThm = procDataDecl("globals_locals_disj")

  //TODO: Boogie proof generation should generate consts_wf and globals_wf theorems in the global theory file instead of the procedure theory file
  val constsWfThm: String = procDataDecl("consts_wf")
  val globalsWfThm: String = procDataDecl("globals_wf")

  val paramsWfThm: String = procDataDecl("params_wf")
  val localsWfThm: String = procDataDecl("locals_wf")

  val paramsDecls : TermIdent = TermIdent(procDataDecl("params_vdecls"))
  val localsDecls : TermIdent = TermIdent(procDataDecl("locals_vdecls"))

  val varContext : Term =
    TermTuple(
      IsaTermUtil.appendList(globalDataAccessor.constDecls, globalDataAccessor.globalDecls),
      IsaTermUtil.appendList(paramsDecls, localsDecls)
    )

  val varContextWfThm: String = procDataDecl("var_context_wf")

  val procBodyAstDef: String = procDataDecl("proc_body")

  val procDef: TermIdent = TermIdent(procDataDecl("ast_proc"))

  val preconditionDef : TermIdent = TermIdent(procDataDecl("pres"))


}

