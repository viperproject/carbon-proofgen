import os
import shutil
import argparse
import pandas as pd
import csv
from pathlib import Path
import typing as typ
from typing import List
from typing import Dict
import statistics


class TestData(typ.NamedTuple):
    file: str
    num_vpr_methods: int
    viper_loc: int
    boogie_loc: int
    isabelle_loc: int
    mean_time: int


def exit_if_is_not_directory(path):
    if (not(os.path.isdir(path))):
        print("The input directory " + path + " does not point to an existing directory.")
        exit(1)

def file_ends_with_type(file_name, type_extension_set):
    name_split = file_name.split('.')
    if len(name_split) < 2:
        return False
    else:
        return name_split[-1] in type_extension_set

def num_uncommented_lines(file_content):
    nonempty_lines = [line for line in file_content if line.strip()]
    return len([line for line in nonempty_lines if not(line.strip(" ").startswith("//"))])

# returns paths as list
def recursively_get_file_paths(input_dir, extension):
    result = []
    for root, dirs, files in os.walk(input_dir):
        for file in files:
            if file.endswith(extension):
                result.append(os.path.join(root, file))
    
    return result

def non_recursively_get_file_paths(input_dir, extension):
    result = []
    for file in os.listdir(input_dir):
        if file.endswith(extension):
            result.append(os.path.join(input_dir, file))
    
    return result

def num_of_isabelle_lines(input_dir):
    num_lines = 0
    has_theories = False
    for root, dirs, files in os.walk(input_dir):        
        for file in files:
            if file.endswith('.thy'):
                has_theories = True
                file_path = os.path.join(root, file)
                file_content = open(file_path, "r")
                nonempty_lines = [line.strip("\n") for line in file_content if line != "\n"]
                num_lines += len(nonempty_lines)
        
    if not(has_theories):
        print("No Isabelle theories for " + input_dir)
        exit(1)
    
    return num_lines

def isabelle_proof_path_correct(input_dir, num_methods):
    if(not(os.path.isdir(input_dir))):
        print("Isabelle proof folder {} does not exist".format(input_dir))
        exit(1)
    
    method_proof_dirs = [x for x in os.listdir(input_dir) if x.startswith("method_proof")]
    if(len(method_proof_dirs) != num_methods):
        print("Number of methods {} and corresponding # proofs in {} ({}) do not match".format(num_methods, input_dir, len(method_proof_dirs)))
        exit(1)
    
    for dir in method_proof_dirs:
        num_theory_files = [x for x in os.listdir(os.path.join(input_dir,dir)) if x.endswith(".thy")]
        if(len(num_theory_files) != 2):
            print("Number of theory files is not 2 for proof folder {}".format(dir))
            exit(1)

    boogie_proofs_dir = os.path.join(input_dir, "boogie_proofs")

    if(not(os.path.isdir(boogie_proofs_dir))):
        print("Boogie proofs folder {} is not a directory".format(boogie_proofs_dir))
        exit(1)

    num_boogie_procedure_proofs = len([x for x in os.listdir(boogie_proofs_dir) if x.endswith("_proofs")])

    if(num_boogie_procedure_proofs != num_methods):
        print("Number of Boogie procedure proofs in {} do not match number of Viper methods {}".format(num_boogie_procedure_proofs, num_methods))
        exit(1)

def write_row_for_test_data(data: TestData, writer):
    writer.writerow([data.file, data.num_vpr_methods, data.viper_loc, data.boogie_loc, data.isabelle_loc, data.mean_time])
    

def compute_statistics(testsuite_name, vpr_files_dir, proofs_dir, output_dir, no_timing) -> List[TestData]:
    vpr_file_paths = recursively_get_file_paths(vpr_files_dir, '.vpr')
    vpr_file_paths.sort(key = lambda p : os.path.basename(p)) # for the output

    if(len(vpr_file_paths) == 0):
        print("There are no Viper files in {}".format(vpr_files_dir))
        exit(1)

    bpl_file_paths = non_recursively_get_file_paths(proofs_dir, '.bpl')
    if(len(bpl_file_paths) != len(vpr_file_paths)):
        print("Test suite {}: The number of Boogie files do not match the number of Viper files.".format(testsuite_name))
        exit(1)

    isabelle_proofs_paths = non_recursively_get_file_paths(proofs_dir, "_proof")
    if(len(isabelle_proofs_paths) != len(vpr_file_paths)):
        print("Test suite {}: The number of Isabelle proofs do not match the number of Viper files.".format(testsuite_name))
        exit(1)
    
    timing_data = None
    
    if(not(no_timing)):
        timing_csv_paths = recursively_get_file_paths(proofs_dir, 'timing_certificates.csv')
        if(len(timing_csv_paths) == 0):
            print("Test suite {}: No timing information".format(testsuite_name))
            exit(1)

        # we pick the newest timing data (lexicographically largest)
        timing_csv_paths.sort(reverse=True)
        
        if(len(timing_csv_paths) > 1):
            print("Testsuite {}: Multiple timing csvs found, picking {}".format(testsuite_name, timing_csv_paths[0]))
    
        timing_data = pd.read_csv(timing_csv_paths[0])
        
        required_columns = ['File','Mean','Median','Pstdev.']
        for col in required_columns:
            if not (col in timing_data):
                print("Test suite {}: {} is not a column in timing csv".format(testsuite_name, col))
                exit(1)

        if (len(timing_data) != len(vpr_file_paths)):
            print("Test suite {}: The number of data rows in the timing csv does not match the number of Viper files.".format(testsuite_name))
            exit(1)

    eval_table_file = os.path.join(output_dir, testsuite_name+"_table.csv")
    
    tests_data = [] 

    with open(eval_table_file, 'w', newline='') as eval_table_file:
        writer = csv.writer(eval_table_file, delimiter=',')
        write_testsuite_title(writer)

        for vpr_path in vpr_file_paths:
            vpr_file_name = os.path.basename(vpr_path)

            bpl_file_name = vpr_file_name.split(".vpr")[0]+".cpg.bpl"

            bpl_path = os.path.join(proofs_dir, bpl_file_name)
            if not(os.path.exists(bpl_path)):
                print("Test suite {}: Boogie file {} for {} does not exist".format(testsuite_name, bpl_path, vpr_file_name))
                exit(1)

            vpr_file_content = [line for line in open(vpr_path,'r')]
            bpl_file_content = [line for line in open(bpl_path,'r')]

            # File information
            num_vpr_methods = len([line for line in vpr_file_content if line.strip(" ").startswith("method ")])
            vpr_loc = num_uncommented_lines(vpr_file_content)
            bpl_loc = num_uncommented_lines(bpl_file_content)

            # Isabelle LOC
            isabelle_proof_suffix = Path(vpr_file_name).stem+"_proof"
            isabelle_proof_candidates = [x for x in isabelle_proofs_paths if x.endswith(isabelle_proof_suffix)]
            if(len(isabelle_proof_candidates) != 1):
                print("Test suite {}: The Isabelle proof folders ending with {} is not 1".format(testsuite_name, isabelle_proof_suffix))
                exit(1)

            isa_proof_path = isabelle_proof_candidates[0]  
            isabelle_proof_path_correct(isa_proof_path, num_vpr_methods)
            isabelle_loc = num_of_isabelle_lines(isa_proof_path)
            
            # Timing
            
            if no_timing:
                mean_time = 0 # choose 0 as mean time if no timing data is provided
            else:
                root_path_suffix = Path(vpr_file_name).stem+"_proof/ROOT"
                proof_timing_row = timing_data[timing_data['File'].str.endswith(root_path_suffix)]
                if(len(proof_timing_row) != 1):
                    print("Test suite {}: timing csv file: the number of rows that end with {} is not 1".format(testsuite_name, root_path_suffix))
                    exit(1)

                proof_timing_row_iloc = proof_timing_row.iloc[0]

                mean_time = proof_timing_row_iloc['Mean']

            data = TestData(file = vpr_file_name,
                            num_vpr_methods = num_vpr_methods,
                            viper_loc = vpr_loc,
                            boogie_loc = bpl_loc,
                            isabelle_loc = isabelle_loc,
                            mean_time = mean_time)
            tests_data.append(data)

            write_row_for_test_data(data, writer)

    return tests_data

def write_testsuite_title(writer):
    writer.writerow(["File", "# Methods", "Viper LOC", "Boogie LOC", "Isabelle LOC", "Proof Check [s]"])

def write_summary_title(writer):
    writer.writerow(["Test suite", 
                    "# Files", 
                    "# Methods", 
                    "# Viper Mean LOC", 
                    "# Boogie Mean LOC",
                    "# Isabelle Mean Loc",
                    "# Proof Check Mean [s]",
                    "# Proof Check Median [s]"])    

def write_summary_row(testsuite, tests_data : List[TestData], writer): 
    files_total = len(tests_data)
    vpr_methods_total = sum([d.num_vpr_methods for d in tests_data])
    vpr_loc_mean =  statistics.mean([d.viper_loc for d in tests_data])
    bpl_loc_mean =  statistics.mean([d.boogie_loc for d in tests_data])
    isabelle_loc_mean =  statistics.mean([d.isabelle_loc for d in tests_data])
    mean_time =  statistics.mean([d.mean_time for d in tests_data])
    median_time =  statistics.median([d.mean_time for d in tests_data])

    writer.writerow([testsuite, 
                    files_total, 
                    vpr_methods_total, 
                    vpr_loc_mean, 
                    bpl_loc_mean,
                    isabelle_loc_mean,
                    mean_time,
                    median_time])    

def compute_summary_table(testsuites_data : Dict[str, List[TestData]], output_dir):
    summary_table_path = os.path.join(output_dir, "summary_table.csv")

    all_data = []

    with open(summary_table_path, 'w', newline='') as summary_table:
        writer = csv.writer(summary_table, delimiter=',')
        write_summary_title(writer)

        for testsuite, tests_data in testsuites_data.items():
            all_data += tests_data
            # summary row for test suite
            write_summary_row(testsuite, tests_data, writer)

        # summary row for the tests in all test suites
        write_summary_row("total", all_data, writer)
    
    selected_table_path = os.path.join(output_dir, "selection_table.csv")
    with open(selected_table_path, 'w', newline='') as selected_table:
        writer = csv.writer(selected_table, delimiter=',')
        write_testsuite_title(writer)

        selected_files = [('viper', 'testHistoryProcesses.vpr'), 
                        ('gobra', 'features__defer__defer-simple-02.gobra.vpr'),
                        ('vercors', 'concepts__parallel__inv-test-fail2.pvl.vpr-0.vpr'),
                        ('MPP', 'banerjee.vpr'),
                        ('MPP', 'darvas.vpr'),
                        ('MPP', 'kusters.vpr')]

        for testsuite, file in selected_files:
            file_data_matches = [data for data in all_data if data.file == file]
            if len(file_data_matches) == 0:
                print("No test data found for {} in test suite {}".format(file, testsuite))
            elif len(file_data_matches) > 1:
                print("Multiple test data found for {} in test suite {}".format(file, testsuite))
            else:
                # length is 1
                write_row_for_test_data(file_data_matches[0], writer)
                

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-i", "--inputdir",        
        help="Directory where the files are located.",
        required=True)
        
    parser.add_argument(
        "-n", "--disabletiming",        
        help="Do not use timing information.",
        action='store_true')


    args = parser.parse_args()

    exit_if_is_not_directory(args.inputdir)

    output_dir = "tables" 

    if(os.path.exists(output_dir)):
        print("Removing existing output folder {}".format(output_dir))
        shutil.rmtree(output_dir)
    
    os.mkdir(output_dir)

    testsuites_data = dict()

    for testsuite_dir_name in os.listdir(args.inputdir):
        testsuite_dir = os.path.join(args.inputdir, testsuite_dir_name)
        vpr_files_dir_name = "vpr_files_for_eval"
        proofs_dir_name = "proofs_for_eval"

        vpr_files_dir = os.path.abspath(os.path.join(testsuite_dir, vpr_files_dir_name))
        proofs_dir = os.path.abspath(os.path.join(testsuite_dir, proofs_dir_name))
    
        if(not(os.path.exists(vpr_files_dir))):
            print("Skipping folder {} since it does not contain the subfolder {} for the Viper files".format(testsuite_dir, vpr_files_dir_name))
            continue
    
        if(not(os.path.exists(proofs_dir))):
            print("Skipping folder {} since it does not contain the subfolder {} for the proofs".format(testsuite_dir, proofs_dir_name))
            continue

        tests_data = compute_statistics(testsuite_dir_name, vpr_files_dir, proofs_dir, output_dir, args.disabletiming)
        testsuites_data[testsuite_dir_name] =  tests_data
    

    compute_summary_table(testsuites_data, output_dir)

if __name__ == '__main__':
    main()