import subprocess

def run_command(command):
    print("Executing command:", " ".join(command))
    result = subprocess.run(command, capture_output=True, text=True)
    output = result.stdout
    print(output)
    return output

def create_user():
    command = [
        "curl",
        "-F", "username=test-user",
        "-F", "password=test-password",
        "http://localhost:3000/dashboard/user",
    ]
    output = run_command(command)
    return output

def login_as_user():
    command = [
        "curl",
        "-F", "username=test-user",
        "-F", "password=test-password",
        "http://localhost:3000/dashboard/login",
    ]
    output = run_command(command)
    return output

def create_repository(login_token):
    command = [
        "curl",
        "-H", f"Authorization: Basic {login_token}",
        "-XPOST",
        f"http://localhost:3000/dashboard/repository?repository-name=test-repo&repository-url-safe-name=test-repo-url-name",
    ]
    output = run_command(command)
    return output

def create_token(repository_id, login_token):
    command = [
        "curl",
        "-H", f"Authorization: Basic {login_token}",
        "-XPOST",
        f"http://localhost:3000/dashboard/repository/{repository_id}/token?permission=readwrite",
    ]
    output = run_command(command)
    return output

def upload_package(repository_id, repo_auth_token):
    command = [
        "curl",
        "-H", f"Authorization: CustomZokkaRepoAuthToken {repo_auth_token}",
        "-F", "elm.json=@test-data/example-elm.json",
        "-F", "docs.json=@test-data/example-docs.json",
        "-F", "README.md=@test-data/example-readme.md",
        "-F", "package.zip=@test-data/example-package.txt",
        f"http://localhost:3000/{repository_id}/upload-package?name=some-author/some-project&version=1.0.0"
    ]
    output = run_command(command)
    return output

def get_all_packages(repository_id, repo_auth_token):
    command = [
        "curl",
        "-H", f"Authorization: CustomZokkaRepoAuthToken {repo_auth_token}",
        f"http://localhost:3000/{repository_id}/all-packages"
    ]
    output = run_command(command)
    return output

def get_dashboard(login_token):
    command = [
        "curl",
        "-H", f"Authorization: Basic {login_token}",
        "http://localhost:3000/dashboard"
    ]
    output = run_command(command)
    return output

if __name__ == "__main__":
    create_user()
    user_login_token = login_as_user()
    repository_id = create_repository(user_login_token)
    repository_auth_token = create_token(repository_id, user_login_token)
    upload_output = upload_package(repository_id, repository_auth_token)
    all_packages_output = get_all_packages(repository_id, repository_auth_token)
    dashboard_output = get_dashboard(user_login_token)
