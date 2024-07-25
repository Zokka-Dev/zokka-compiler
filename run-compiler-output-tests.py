import subprocess
import os
import timeit

def run_zokka_make(zokka_cmd, file_to_make, project_dir):
    return subprocess.run([zokka_cmd, "make", file_to_make], cwd=project_dir)

if __name__ == "__main__":

    start_time = timeit.default_timer()

    find_zokka_cmd = ["cabal", "list-bin", "zokka"]

    
    zokka_exec_location = \
        subprocess.run(find_zokka_cmd, capture_output=True).stdout.strip()

    run_zokka_make_cmd = [zokka_exec_location, ]

    current_dir = "."

    top_level_tests_dir = os.path.join(current_dir, "compiler-output-tests")

    print(f"=========\nRunning compiler-output tests found in {top_level_tests_dir}\n=========\n")

    bad_occurs_check_test_0 =\
        run_zokka_make(zokka_exec_location, os.path.join("src", "BadOccursCheck.elm"), top_level_tests_dir)

    if bad_occurs_check_test_0.returncode == 0:
        raise Exception("Our bad occurs check failed! The compiler succeeded when it should have failed!")

    bad_occurs_check_test_1 =\
        run_zokka_make(zokka_exec_location, os.path.join("src", "BadOccursCheck1.elm"), top_level_tests_dir)

    if bad_occurs_check_test_1.returncode == 0:
        raise Exception("Our bad occurs check failed! The compiler succeeded when it should have failed!")

    total_test_duration = timeit.default_timer() - start_time

    print(f"=========\nTotal test duration: {total_test_duration} seconds\n=========\n")
