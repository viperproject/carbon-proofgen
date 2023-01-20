package isabelle.ast

object IsaUtil {

  def qualifyName(theoryName: String, name: String) : String =  theoryName + "." +name

  def definitionLemmaFromName(defName: String) : String = defName+"_def"

}
