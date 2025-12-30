import argparse
import copy
import json
import os
import shutil
import stat
import subprocess

# Expect NPM_TOKEN as environment variable rather than argument to prevent the
# token from showing up in build logs

parser = argparse.ArgumentParser(
    prog="publish-to-npm",
    description="Publishes npm packages",
)

parser.add_argument("-w", "--windows-x86-binary-source-location", required=True)
parser.add_argument("-d", "--darwin-x86-binary-source-location", required=True)
parser.add_argument("-l", "--linux-x86-binary-source-location", required=True)
parser.add_argument("-a", "--darwin-arm64-binary-source-location", required=True)
parser.add_argument("-i", "--linux-arm64-binary-source-location", required=True)
parser.add_argument("-e", "--new-version", required=True)
parser.add_argument("-n", "--npm-dry-run", action=argparse.BooleanOptionalAction)

args = parser.parse_args()

windows_binary_source_location = args.windows_x86_binary_source_location
darwin_x86_binary_source_location = args.darwin_x86_binary_source_location
darwin_arm64_binary_source_location = args.darwin_arm64_binary_source_location
linux_x86_binary_source_location = args.linux_x86_binary_source_location
linux_arm64_binary_source_location = args.linux_arm64_binary_source_location
new_version = args.new_version
try:
    dry_run = args.npm_dry_run 
except AttributeError:
    dry_run = False


def rewrite_version_of_package_json(package_json, version):
    package_json_copy = copy.deepcopy(package_json)
    package_json_copy["version"] = version
    return package_json_copy


def rewrite_versions_of_optional_dependencies(package_json, version):
    package_json_copy = copy.deepcopy(package_json)
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-darwin_x64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-darwin_arm64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-linux_x64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-win32_x64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-linux_arm64"] = version
    return package_json_copy


try:
    os.environ["NPM_TOKEN"]
except KeyError:
    raise Exception("The NPM_TOKEN environment variable must be set otherwise we cannot publish to npm!")

top_level_npm_directory = "./installers/npm/"
darwin_x86_directory = "./installers/npm/packages/darwin_x64/"
darwin_arm64_directory = "./installers/npm/packages/darwin_arm64/"
windows_directory = "./installers/npm/packages/win32_x64/"
linux_x86_directory = "./installers/npm/packages/linux_x64/"
linux_arm64_directory = "./installers/npm/packages/linux_arm64/"

def copy_and_chmod_file(source, destination):
    try:
        executable = destination
        shutil.copyfile(source, executable)
        current_stat = os.stat(executable)
        os.chmod(executable, current_stat.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

    except shutil.SameFileError:
        print(f"You are asking to copy {source} to {destination}, which are just the exact same file path, so no copying is taking place and just leave the file as-is.")
        pass

copy_and_chmod_file(darwin_x86_binary_source_location, darwin_x86_directory + "/zokka")
copy_and_chmod_file(darwin_arm64_binary_source_location, darwin_arm64_directory + "/zokka")
copy_and_chmod_file(windows_binary_source_location, windows_directory + "/zokka.exe")
copy_and_chmod_file(linux_x86_binary_source_location, linux_x86_directory + "/zokka")
copy_and_chmod_file(linux_arm64_binary_source_location, linux_arm64_directory + "/zokka")

additional_npm_args = [ "--dry-run" ] if dry_run else []

for directory in [darwin_x86_directory, darwin_arm64_directory, windows_directory, linux_x86_directory]:
    with open(directory + "package.json", "r+") as f:
        package_json = json.load(f)
        new_package_json = rewrite_version_of_package_json(package_json, new_version)
        f.seek(0)
        json.dump(new_package_json, f, indent=2)
        f.truncate()
    if "alpha" in new_version:
        subprocess.run(["npm", "publish", "--tag", "alpha"] + additional_npm_args, cwd=directory)
    elif "beta" in new_version:
        subprocess.run(["npm", "publish", "--tag", "beta"] + additional_npm_args, cwd=directory)
    else:
        subprocess.run(["npm", "publish"] + additional_npm_args, cwd=directory)


with open(top_level_npm_directory + "package.json", "r+") as f:
    package_metadata = json.load(f)
    new_package_metadata = \
        rewrite_version_of_package_json(rewrite_versions_of_optional_dependencies(package_metadata, new_version), new_version)
    f.seek(0)
    json.dump(new_package_metadata, f, indent=2)
    f.truncate()

if "alpha" in new_version:
    subprocess.run(["npm", "publish", "--tag", "alpha"] + additional_npm_args, cwd=top_level_npm_directory)
    # Apparently npm doesn't allow multiple flags at once with publish, so
    # we need to manually add latest with npm-dist-tag
    # Note that according to
    # https://docs.npmjs.com/cli/v9/commands/npm-publish#dry-run dist-tag does
    # not honor the --dry-run flag so we have to do it manually
    if dry_run:
        print("Skipping setting published package as latest version because this is a dry-run.")
    else:
        subprocess.run(["npm", "dist-tag", "add", f"zokka@{new_version}", "latest"] + additional_npm_args, cwd=top_level_npm_directory)
elif "beta" in new_version:
    subprocess.run(["npm", "publish", "--tag", "beta"] + additional_npm_args, cwd=top_level_npm_directory)
    # Note that according to
    # https://docs.npmjs.com/cli/v9/commands/npm-publish#dry-run dist-tag does
    # not honor the --dry-run flag so we have to do it manually
    if dry_run:
        print("Skipping setting published package as latest version because this is a dry-run.")
    else:
        subprocess.run(["npm", "dist-tag", "add", f"zokka@{new_version}", "latest"] + additional_npm_args, cwd=top_level_npm_directory)
else:
    subprocess.run(["npm", "publish"] + additional_npm_args, cwd=top_level_npm_directory)
