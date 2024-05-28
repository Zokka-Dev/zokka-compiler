import json
import subprocess
import threading
import time
import argparse

def run_command(command):
    print("Executing command:", " ".join(command))
    result = subprocess.run(command, capture_output=True, text=True)
    output = result.stdout
    stderroutput = result.stderr
    if result.returncode != 0:
        raise Exception(f"Command '{command}' did not complete successfully!  Stdout: {output} Stderr: {stderroutput}")
    print(output)
    return output

def create_user():
    command = [
        "curl",
        "--fail-with-body",
        "-F", "username=test-user",
        "-F", "password=test-password",
        "http://localhost:3000/dashboard/user",
    ]
    output = run_command(command)
    return output

def login_as_user():
    command = [
        "curl",
        "--fail-with-body",
        "-F", "username=test-user",
        "-F", "password=test-password",
        "http://localhost:3000/dashboard/login",
    ]
    output = run_command(command)
    return output

def create_repository(login_token):
    command = [
        "curl",
        "--fail-with-body",
        "-H", f"Authorization: Basic {login_token}",
        "-XPOST",
        f"http://localhost:3000/dashboard/repository?repository-name=test-repo&repository-url-safe-name=test-repo-url-name",
    ]
    output = run_command(command)
    return output

def create_token(repository_id, login_token):
    command = [
        "curl",
        "--fail-with-body",
        "-H", f"Authorization: Basic {login_token}",
        "-XPOST",
        f"http://localhost:3000/dashboard/repository/{repository_id}/token?permission=readwrite",
    ]
    output = run_command(command)
    return output

def upload_package(repository_id, repo_auth_token):
    command = [
        "curl",
        "--fail-with-body",
        "-H", f"Authorization: CustomZokkaRepoAuthToken {repo_auth_token}",
        "-F", "elm.json=@test-data/example-elm.json",
        "-F", "docs.json=@test-data/example-docs.json",
        "-F", "README.md=@test-data/example-readme.md",
        "-F", "package.zip=@test-data/example-package.txt",
        f"http://localhost:3000/api/{repository_id}/upload-package?name=some-author/some-project&version=1.0.0"
    ]
    output = run_command(command)
    return output

def get_all_packages(repository_id, repo_auth_token):
    command = [
        "curl",
        "--fail-with-body",
        "-H", f"Authorization: CustomZokkaRepoAuthToken {repo_auth_token}",
        f"http://localhost:3000/api/{repository_id}/all-packages"
    ]
    output = run_command(command)
    return output

def get_dashboard(login_token):
    command = [
        "curl",
        "--fail-with-body",
        "-H", f"Authorization: Basic {login_token}",
        "http://localhost:3000/dashboard/all-data"
    ]
    output = run_command(command)
    return output

SHOULD_SHUTDOWN_SERVER = False
SQLITE_DATABASE_FILENAME = "temp_testing_package_db.db"

def run_server_in_background(skip_initial_cabal_build=False):
    # hack until we figure out what's causing things to hang
    # this ensures that we don't have to wait for things to compile and
    # therefore that our sleep of 3 seconds is sufficient
    if not skip_initial_cabal_build:
        run_command(["cabal", "build"])
    if not skip_initial_cabal_build:
        command = [
            f"cabal run myPackage -- --port=3000 --database-file={SQLITE_DATABASE_FILENAME} --initialization-script=initialize_tables.sql --external-website-url=https://localhost:3000",
        ]
    else:
        location_of_zokka_executable = run_command(["cabal", "list-bin", "myPackage"]).rstrip()
        command = [f"{location_of_zokka_executable} --port=3000 --database-file={SQLITE_DATABASE_FILENAME} --initialization-script=initialize_tables.sql --external-website-url=https://localhost:3000"]
    def target(setup_event, shutdown_event):
        process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,)
        print(f"Executing {command}")
        time.sleep(3)
        setup_event.set()
        # For some reason the following just hangs forever. Don't know why.
        #for stdout_line in iter(process.stdout.readline, ''):
            #print(stdout_line, end='')
            #if "Setting phasers to stun" in stdout_line:
                #setup_event.set()
                #break
        shutdown_event.wait()
        process.terminate()

    server_has_setup_event = threading.Event()
    should_shutdown_server_event = threading.Event()
    thread = threading.Thread(target=target, args=(server_has_setup_event,should_shutdown_server_event))
    thread.start()
    return (thread, server_has_setup_event, should_shutdown_server_event)

def delete_database_file():
    command = ["rm", "-f", SQLITE_DATABASE_FILENAME]
    output = run_command(command)
    return output

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="Test Script for Zokka Custom Package", 
        description="Tests the Zokka custom package server with basic smoke tests",
    )
    parser.add_argument("-s", "--skip-initial-cabal-build", action="store_true")
    args = parser.parse_args()
    try:
        (server_thread, server_has_setup_event, should_shutdown_server_event) =\
            run_server_in_background(args.skip_initial_cabal_build)
        server_has_setup_event.wait()
        create_user()
        user_login_token = login_as_user()
        repository_id = create_repository(user_login_token)
        repository_auth_token_as_json_str = create_token(repository_id, user_login_token)
        repository_auth_token = json.loads(repository_auth_token_as_json_str)["value"]
        upload_output = upload_package(repository_id, repository_auth_token)
        all_packages_output = get_all_packages(repository_id, repository_auth_token)
        dashboard_output = get_dashboard(user_login_token)
        should_shutdown_server_event.set()
        server_thread.join()
    finally:
        if "should_shutdown_server_event" in locals():
            should_shutdown_server_event.set()
        delete_database_file()
