package viper.carbon.proofgen

import isabelle.ast.{IsaPrettyPrinter, Theory}

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

object StoreTheory {
  def storeTheories(theories: Seq[Theory], rootDir: Path) =
    theories.foreach(thy => storeTheory(thy, rootDir))

  def storeTheory(theory: Theory, rootDir: Path) = {
      val theoryString = IsaPrettyPrinter.prettyPrint(theory)
      Files.write(rootDir.resolve(theory.theoryName+".thy"), theoryString.getBytes(StandardCharsets.UTF_8))
  }

}


