package viper.carbon.proofgen

import isabelle.ast.IsaPrettyPrinter
import isabelle.{ast => isa}
import viper.silver.{ast => sil}

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object StoreTheory {

  def storeMethodInIsaTheory(m: sil.Method) = {
    m.body match {
      case Some(mBody) =>
        val varTranslation = DeBruijnTranslation.freshTranslation(m.formalArgs map (varDecl => varDecl.localVar))
        val mBodyTerm = ViperToIsa.translateStmt(mBody)(varTranslation)

        val definition = isa.DefDecl("viper_stmt", ViperIsaType.stmt, (Seq(), mBodyTerm))

        val theory = isa.Theory(m.name, Seq("Viper.ViperLang"), Seq(definition))

        val theoryString = IsaPrettyPrinter.prettyPrint(theory)
        Files.write(Paths.get(System.getProperty("user.dir")).resolve(theory.theoryName+".thy"), theoryString.getBytes(StandardCharsets.UTF_8) )
      case None =>
    }
  }

}
