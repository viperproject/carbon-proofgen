package isabelle.ast

object IsaUtil {

  def qualifyName(theoryName: String, name: String) : String =  theoryName + "." +name

  def definitionLemmaFromName(defName: String) : String = defName+"_def"

  /***
    *
    * @param id desired identifier
    * @return valid Isabelle identifier that resembles the argument
    */
  def convertToValidIsabelleIdentifier(id: String) : String = {
    if(id.isEmpty) {
      "empty_id"
    } else {
      if(!id(0).isLetter) {
        "a"+id
      } else {
        id
      }
    }
  }

}
