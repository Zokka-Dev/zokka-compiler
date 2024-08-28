import subprocess
import os
import timeit

def run_zokka_make(zokka_cmd, file_to_make, project_dir):
    return subprocess.run([zokka_cmd, "make", file_to_make], cwd=project_dir, capture_output=True)

def run_compilation_tests(expected_return_code: int, filepath):
    top_level_tests_dir = filepath

    top_level_tests_src_dir = os.path.join(top_level_tests_dir, "src")

    print(f"=========\nRunning compiler-output tests (expecting return code of {expected_return_code}) found in {top_level_tests_dir}\n=========\n")

    test_files = os.listdir(top_level_tests_src_dir)

    for test_file in test_files:
        test_result =\
            run_zokka_make(zokka_exec_location, os.path.join("src", test_file), top_level_tests_dir)

        if test_result.returncode != expected_return_code:
            if test_result.returncode != 0:
                print(test_result.stderr.decode("utf-8"))
            raise Exception(f"We failed when testing {test_file}! The compiler had a return code of {test_result.returncode} when we expected {expected_return_code}")

    total_test_duration = timeit.default_timer() - start_time

    print(f"=========\nTotal test duration: {total_test_duration} seconds\n=========\n")

if __name__ == "__main__":

    start_time = timeit.default_timer()

    find_zokka_cmd = ["cabal", "list-bin", "zokka"]

    
    zokka_exec_location = \
        subprocess.run(find_zokka_cmd, capture_output=True).stdout.strip()

    run_zokka_make_cmd = [zokka_exec_location, ]

    current_dir = "."

    expected_failed_compilation_path = os.path.join(current_dir, "compiler-output-tests", "expect-failed-compilation")

    run_compilation_tests(expected_return_code=1, filepath=expected_failed_compilation_path)

    expected_successful_compilation_path = os.path.join(current_dir, "compiler-output-tests", "expect-successful-compilation")

    run_compilation_tests(expected_return_code=0, filepath=expected_successful_compilation_path)
