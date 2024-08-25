import os
import shutil
import subprocess
import argparse
from pathlib import Path
from shutil import which

# This is a slightly-modified version of the proof generation script from the artifact
# https://zenodo.org/records/10802176 from "Towards Trustworthy Automated
# Program Verifiers: Formally Validating Translations into an Intermediate
# Verification Language" by Parthasarathy, et al.
# It has been modified to run a built 'carbon.jar' on the provided set of benchmarks
# This script is invoked with command:
# ```
# $ python scripts/generate_proofs_artifact.py --inputdir cpg_old_expressions_benchmarks --viperproofExe viper_proof_no_param --boogieproofExe $BOOGIE_EXE
# ````
# from the project root

PROJECT_ROOT = Path(__file__).parent.parent

CARBON_JAR_FILE = PROJECT_ROOT / 'target' / 'scala-2.13' / 'carbon.jar'

def get_num_vpr_files(input_dir):
    return len ([f for dp, dn, fn in os.walk(input_dir) for f in fn])

def generate_proofs(testsuite, input_dir, output_dir, carbon_proofgen_bin, boogie_proofgen_bin) -> bool:
    n_success_verified = 0
    n_success_not_verified = 0
    n_not_supported = 0
    n_unknown_error = 0

    # turn input directory path into an absolute path, since we are going to 
    # change the working directory
    input_dir_absolute = os.path.abspath(input_dir)    

    n_vpr_files = get_num_vpr_files(input_dir_absolute)

    print("==============Starting proof generation for {} ({} Viper files)==============".format(testsuite, n_vpr_files))

    if(os.path.exists(output_dir)):
        print("Removing existing folder {}".format(output_dir))
        shutil.rmtree(output_dir)
    
    # store current work directory such that can change back to it
    working_dir_at_start = os.getcwd() 

    # change to output directory
    os.mkdir(output_dir)
    os.chdir(output_dir)

    for root, dirs, files in os.walk(input_dir_absolute):
        for file in files:
            if file.endswith('.vpr'):
                viper_file_path = os.path.join(root, file)

                with open(viper_file_path) as f:
                    boogie_file_name = file.split(".vpr")[0]+".cpg.bpl"

                    options = ["--genProofs", "--desugarPolymorphicMaps", "--disableAllocEncoding", "--print", boogie_file_name, "--boogieExe", boogie_proofgen_bin]

                    errorcode = subprocess.run(['java', '-jar', str(CARBON_JAR_FILE)] + options + [viper_file_path], stdout=subprocess.DEVNULL)

                    if(errorcode.returncode == 0):
                        print("Generated proofs for: {}".format(file))
                        print("Verification successful for: {}".format(file))
                        n_success_verified += 1
                    elif(errorcode.returncode == 1):
                        print("Generated proofs for: {}".format(file))
                        print("Verification failed for: {}".format(file))
                        # program does not verify, but proof should be generated
                        n_success_not_verified += 1
                    elif(errorcode.returncode == 17):
                        print("Test not supported by proof generation: " + file)
                        n_not_supported += 1
                    else:
                        print("Unknown error code ({}) for: {}".format(errorcode.returncode, file))
                        n_unknown_error += 1
    
    print()

    n_success = n_success_verified + n_success_not_verified
    print("Generated proofs for {} tests ({} of these tests are verified by Viper)".format(n_success, n_success_verified))

    if(n_not_supported > 0):
        print("{} tests are not supported for proof generation".format(n_not_supported))

    if(n_unknown_error > 0):
        print("{} tests yielded an unknown error code".format(n_unknown_error))

    # check that the reported number of proofs is reflected by the created directories
    num_proof_dirs = [d for d in os.listdir() if os.path.isdir(d) and d.endswith("proof")]
    consistent = (len(num_proof_dirs) == n_success)
    if(not(consistent)):
        print("INCONSISTENCY: Although {} proofs were reported to be generated, only {} proof directories were created".format(n_success, len(num_proof_dirs)))
    
    print()
    
    os.chdir(working_dir_at_start)

    return (n_success == n_vpr_files and consistent)

def main():
    parser = argparse.ArgumentParser(allow_abbrev=False)

    parser.add_argument(
        "-i", "--inputdir",        
        help="Directory where all Carbon files are located, for which proofs should be generated.",
        required=True)

    parser.add_argument(
        "-v", "--viperproofExe",
        help="Path to Viper proof generation executable (must be an absolute path or a command that can be executed from any directory)",
        required=True
    )

    parser.add_argument(
        "-b", "--boogieproofExe",
        help="Path to Boogie proof generation executable (must be an absolute path or a command that can be executed from any directory)",
        required=True
    )

    args = parser.parse_args()

    if (not(os.path.isdir(args.inputdir))):
        print("The input directory " + args.inputdir + " does not point to an existing directory.")
        exit(1)
    
    all_success = True

    for testsuite_dir_name in os.listdir(args.inputdir):
        testsuite_dir = os.path.join(args.inputdir, testsuite_dir_name)
        vipertests_dir = os.path.join(testsuite_dir, "vpr_files_for_eval")
        outputdir = os.path.join(testsuite_dir, "proofs_for_eval")
        
        success_testsuite = generate_proofs(testsuite_dir_name,
                            vipertests_dir, 
                            outputdir,
                            args.viperproofExe, 
                            args.boogieproofExe
                        )
        if(not(success_testsuite)):
            print("Could not proofs for all files in {}".format(testsuite_dir_name))
        
        all_success = all_success and success_testsuite
    
    if all_success:
        print()
        print("Successfully generated proofs for all files")
    else:
        print()
        print("Could not generate proofs for all files")

if __name__ == '__main__':
    main()
