package viper.carbon.proofgen

import isabelle.ast.{IsaPrettyPrinter, Theory}

import java.nio.file.{Files, Path, Paths}
import viper.silver.{ast => sil}
import isabelle.{ast => isa}
import viper.carbon.boogie.Procedure
import viper.carbon.proofgen.hints.StmtProofHint
import viper.carbon.verifier.Environment

import java.nio.charset.StandardCharsets
import scala.collection.mutable

//a directory with path proofGenDir must exist
class ProofGenInterface(val proofDir: Path) {

  val boogieProofDirName : String = "boogie_proofs"
  val boogieProofDir : Path = proofDir.resolve(boogieProofDirName)

  /***
    * Creates and stores the Isabelle proof relating the Viper and Boogie programs
    *
    * Should only be invoked if Boogie has already been run and produced a proof at [[boogieProofDir()]]
    */
  def finishProof(p: sil.Program) : Unit = {
    if(!Files.exists(boogieProofDir)) {
      sys.error(s"Cannot generate CPG proof: Boogie proofs are not stored at ${boogieProofDir.toAbsolutePath.toString}")
    }
  }

  def methodProofPath(m: sil.Method) : Path =
    proofDir.resolve("method_proof"+m.name)

  def methodProgTheory(m: sil.Method) : String = m.name+"_vpr_prog"

  def generateProofForMethod(m: sil.Method, procBpl: Procedure, procBplEnv: Environment, bodyProofHint: StmtProofHint) = {
    m.body match {
      case Some(_) =>
        val dir: Path = Files.createDirectory(methodProofPath(m))

        /* Viper program representation */
        val varTranslation = DeBruijnTranslation.freshTranslation((m.formalArgs ++ m.formalReturns) map (varDecl => varDecl.localVar))

        val (theory, mAccessor) = IsaVprProgramGenerator.isaProgramRepr(m, methodProgTheory(m), varTranslation)
        StoreTheory.storeTheory(theory, dir)

        /* Viper <-> Boogie proof */
        //here we are assuming how Boogie proof generation names proof folders
        //TODO: does not work if there are name clashes between namespaces --> pretty printer might not use given name
        val bplProgTheoryPath =
          s"../${boogieProofDirName}/${procBpl.name.name}_proofs/${procBpl.name.name}_before_ast_to_cfg_prog"

        val bplProcAccessor = new IsaBoogieProcAccessor(
          procBpl,
          procBplEnv,
          bplProgTheoryPath)

        val methodProofGenerator = MethodProofGenerator(
          "relational_proof",
          mAccessor,
          varTranslation,
          bplProcAccessor)

        val relationalProofTheory = methodProofGenerator.generateProof()

        StoreTheory.storeTheory(relationalProofTheory, dir)
      case None =>
        //TODO: pre- and postconditions still matter for abstract methods
    }
  }




}
