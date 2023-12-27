import subprocess
import os
import timeit

if __name__ == "__main__":

    start_time = timeit.default_timer()

    find_zokka_cmd = ["cabal", "list-bin", "zokka"]

    
    zokka_exec_location = \
        subprocess.run(find_zokka_cmd, capture_output=True).stdout.strip()

    run_elm_test_cmd = ["npx", "elm-test-rs", "--compiler", zokka_exec_location]

    current_dir = "."

    top_level_tests_dir = os.path.join(current_dir, "elm-test-rs-tests")

    print(f"=========\nRunning top-level tests found in {top_level_tests_dir}\n=========\n")

    elm_test_result = subprocess.run(run_elm_test_cmd, cwd=top_level_tests_dir)

    if not elm_test_result.returncode == 0:
        raise Exception("Our overall elm test suite failed!")

    existing_elm_code_to_test_dir = os.path.join(top_level_tests_dir, "pre-existing-elm-projects")

    for dir_entry in os.scandir(existing_elm_code_to_test_dir):
        name = dir_entry.name
        project_dir = os.path.join(existing_elm_code_to_test_dir, name)
        print(f"=========\nRunning tests found in {project_dir}\n=========\n")
        elm_test_result = subprocess.run(run_elm_test_cmd, cwd=project_dir)
        if not elm_test_result.returncode == 0:
            raise Exception(f"We failed while running tests in {project_dir}")

    total_test_duration = timeit.default_timer() - start_time

    print(f"=========\nTotal test duration: {total_test_duration} seconds\n=========\n")
