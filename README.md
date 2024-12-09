# Proof-Producing Fork of the Viper-to-Boogie Translation
This folder contains a proof-producing instrumentation of the 
[Viper-to-Boogie translation](https://github.com/viperproject/carbon) for a subset of the 
[Viper intermediate verification language](http://www.pm.inf.ethz.ch/research/viper.html).

### Installation Dependencies
* sbt (Scala build tool): see the [main website](https://www.scala-sbt.org/) for the installation instructions
* Z3 version 4.8.7: you can get the binary from the [Z3 releases](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.7)
  * make sure the folder containing the binary is included in the `PATH` environment variable (required for Boogie)
* Boogie version that embeds Boogie programs in Isabelle:
  The correct version is included in the `boogie-proofgen` folder.
  To install it, you need to install the .NET Core 6 SDK and then compile the project using

  ```dotnet build -c Release boogie-proofgen/Source/Boogie.sln```

  After the compilation, the corresponding executable `BoogieDriver` (for Linux and Mac) or 
  `BoogieDriver.exe` is located in `boogie-proofgen/Source/BoogieDriver/bin/Release/net6.0`.

  Our proof-producing instrumentation uses the executable to obtain an Isabelle 
  embedding of the Boogie AST (proofs for the Boogie pipeline are not generated) 
  and to run verification on the Boogie program.

  Note that if the Boogie program does not verify, then the error messages reported by 
  our tool are currently not yet as expected, since the Boogie version producing the Isabelle
  embedding removes some of the error reporting features. 

### Set Environment variables
The Carbon verifier supports command line options `--z3Exe` and `--boogieExe` to provide
explicit paths to the Z3 and Boogie executables.
If you do not want to provide those paths explicitly, then set the `Z3_EXE` and 
`BOOGIE_EXE` environment variables to point to the Z3 executable and the Boogie proof generation executable, respectively. 

### Run Tool

There are multiple alternatives:
* Alternative 1: Compile and run with sbt
  `sbt "run --genProofs --desugarPolymorphicMaps --disableAllocEncoding <path to Viper file>"`
*  Alternative 2: Compile with sbt using `sbt compile` and then use `carbon.sh` or 
   `carbon.bat` directly:

   `carbon.sh --genProofs --desugarPolymorphicMaps --disableAllocEncoding <path to Viper file`
* Alternatively, for a faster startup without compilation each time, build a fat JAR 
using `sbt assembly` and then run directly using `java`:  

  `java -jar ./target/scala-*/carbon.jar --genProofs --desugarPolymorphicMaps --disableAllocEncoding <path to Viper file>`

Use the additional options `--z3Exe` and `--boogieExe` if you have not set up
the environment variables described above or want to provide custom versions of
Boogie or Z3.

Note that we have created a shell script `viper_proof` that takes the Viper program as input
and then executes this `java -jar` command. For the `viper_proof` command to work, you need 
to set up the `BOOGIE_EXE` and `Z3_EXE` environment variables (see above).

Running the tool creates a folder with the proofs in the working directory.
There is one directory per method and one directory for the Boogie embedding.
The file `end_to_end_proof.thy` at the top level contains the full proof. An 
Isabelle ROOT session file is also generated at the top level.

Additionally, you can provide the `--print nameToBoogieFile.bpl` option (before providing
the Viper file) to store the generated Boogie program in the file `nameToBoogieFile.bpl`.

### Dependencies for checking generated proofs

To check the generated proofs, you need to install Isabelle 2022 and install 
Isabelle dependencies for the Boogie formalization and the Viper total heap 
formalization. You can do so directly via the `viper-roots` subfolder 
by running the following commands:

`isabelle components -u viper-roots/foundational-boogie/BoogieLang`

`isabelle components -u viper-roots/vipersemcommon`

`isabelle components -u viper-roots/viper-total-heaps`
