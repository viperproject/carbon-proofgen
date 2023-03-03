package viper.carbon.proofgen.functions

import viper.carbon.boogie._
import viper.carbon.modules.{HeapModule, PermModule}
import viper.silver.{ast => sil}

import scala.collection.mutable

class FunctionProofGenInterface(heapModule: HeapModule, permModule: PermModule) {

  lazy val boogieFunctionNames: Map[BoogieFun, Seq[Identifier]] = {

    val dummyFieldAccess = sil.FieldAccess(sil.IntLit(0)(), sil.Field("f", sil.Int)())()

    Map.from(
      Seq(
        (HeapRead, functionNameFromLookup(heapModule.translateLocationAccess(dummyFieldAccess))),
        (HeapStore, functionNameFromLookup(heapModule.currentHeapAssignUpdate(dummyFieldAccess, IntLit(0)))),
        (MaskRead, functionNameFromLookup(permModule.currentPermission(dummyFieldAccess))),
        //the perm module current does not expose updating the mask, so we directly just use the mask store identifier
        (MaskStore, Seq(permModule.pmaskTypeDesugared.storeId))
      )
    )
  }

  /**
    *
    * @param boogieFun Boogie function
    * @return all Boogie function lookup lemmas related to the input Boogie function
    */
  def funBplLookupLemmas(boogieFun: BoogieFun) : Seq[String]=
  {
    boogieFun match {
      case ViperFun(s) =>
        //here we assume that for each Viper function the corresponding Boogie function has the same name
        Seq("mfun"+s)
      case _ =>
        boogieFunctionNames.get(boogieFun) match {
          case None => sys.error(s"Could not find $boogieFun")
          //here we assume that used identifiers are mapped to their names in the Boogie program
          case Some(ids) => ids.map(id => "mfun"+id.name)
        }
    }
  }

  /**
    * @param vprFunctions relevant Viper functions
    * @return all Boogie function lookup lemmas for functions that could appear in Boogie expressions corresponding to
    *         Viper expressions (where the first argument provides the Viper functions that could appear in the Viper expression)
    */
  def allFunBplLookupLemmasForViperExpressions(vprFunctions: Seq[sil.Function]) : Seq[String] = {
    (vprFunctions.map(f => ViperFun(f.name)) ++ Seq(HeapRead, MaskRead)).flatMap(funBplLookupLemmas)
  }


  private def functionNameFromLookup(boogieNode: Node) : Seq[Identifier] =
  {
    Visitor.reduce[Seq[Identifier]](boogieNode)(
      {
        case (FuncApp(id, _, _), identifiersSeq) =>
          id +: identifiersSeq.flatten
        case (_, identifiersSeq) => identifiersSeq.flatten
      }
    )
  }

}

sealed trait BoogieFun

case class ViperFun(s: String) extends BoogieFun
case object HeapRead extends BoogieFun
case object HeapStore extends BoogieFun
case object MaskRead extends BoogieFun
case object MaskStore extends BoogieFun
