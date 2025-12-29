import subprocess
import os
import shutil
import timeit

def custom_package_repository_config(repository_filepath):
    return f"""
{{
    "repositories": [
        {{
            "repository-type": "local-directory-of-files-readonly-mirror-of-package-server-v1.0",
            "repository-file-path": "{repository_filepath}"
        }}
    ],
    "single-package-locations": []
}}
    """

def run_zokka_make(zokka_cmd, file_to_make, project_dir, elm_home: str | None = None):
    env = os.environ
    if elm_home:
        env = env | {"ELM_HOME": elm_home}
    return subprocess.run([zokka_cmd, "make", file_to_make], cwd=project_dir, capture_output=True, env=env)

if __name__ == "__main__":

    start_time = timeit.default_timer()

    print(f"=========\nRunning custom repository tests\n=========\n")

    find_zokka_cmd = ["cabal", "list-bin", "zokka"]

    zokka_exec_location = \
        subprocess.run(find_zokka_cmd, capture_output=True).stdout.strip()

    # Having a single test output lets us easily wipe/cleanup a test and have a
    # place for .gitignore
    test_output_location = os.path.join("custom-repo-tests", "test-output")

    # Clear the output if it exists already
    if os.path.exists(test_output_location):
        shutil.rmtree(test_output_location)

    # This is technically not needed when we make the repos config location
    # later, but this is good for cleanliness and in case we want to move that
    # location elsewhere later
    os.makedirs(test_output_location)

    # Make this an absolute path because we'll be passing it around to different
    # relative environments
    elm_home_location = os.path.abspath(os.path.join(test_output_location, "elm-home"))

    zokka_custom_repos_config_location = os.path.join(elm_home_location, "0.19.1", "zokka")

    os.makedirs(zokka_custom_repos_config_location)

    config_json_path = os.path.join(zokka_custom_repos_config_location, "custom-package-repository-config.json")

    with open(config_json_path, "w") as f:
        mirror_path = os.path.abspath(os.path.join("custom-repo-tests", "mirror"))
        f.write(custom_package_repository_config(mirror_path))

    elm_todomvc = os.path.join("custom-repo-tests", "elm-todomvc")

    # We'll copy the known Elm code to another directory to start fresh each
    # time
    
    elm_todomvc_test = os.path.join(test_output_location, "elm-todomvc")

    if os.path.exists(elm_todomvc_test):
        shutil.rmtree(elm_todomvc_test)

    shutil.copytree(elm_todomvc, elm_todomvc_test)

    file_to_build = os.path.join("src", "Main.elm")

    test_result = run_zokka_make(zokka_exec_location, file_to_build, elm_todomvc_test, elm_home=elm_home_location)

    if test_result.returncode != 0:
        print(test_result.stderr.decode("utf-8"))
        raise Exception(f"We failed when trying to build {file_to_build} in {elm_todomvc_test}")

    total_test_duration = timeit.default_timer() - start_time

    print(f"=========\nTotal test duration: {total_test_duration} seconds\n=========\n")
