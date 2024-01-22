import argparse
import copy
import json
import os
import shutil
import subprocess

# Expect NPM_TOKEN as environment variable rather than argument to prevent the
# token from showing up in build logs

parser = argparse.ArgumentParser(
    prog="publish-to-npm",
    description="Publishes npm packages",
)

parser.add_argument("-w", "--windows-binary-source-location")
parser.add_argument("-d", "--darwin-binary-source-location")
parser.add_argument("-l", "--linux-binary-source-location")
parser.add_argument("-e", "--new-version")

args = parser.parse_args()

windows_binary_source_location = args.windows_binary_source_location
darwin_binary_source_location = args.darwin_binary_source_location
linux_binary_source_location = args.linux_binary_source_location
new_version = args.new_version

if "alpha" in new_version:
    additional_npm_flags = ["--tag", "alpha"]
elif "beta" in new_version:
    additional_npm_flags = ["--tag", "beta"]
else:
    additional_npm_flags = []

def rewrite_version_of_package_json(package_json, version):
    package_json_copy = copy.deepcopy(package_json)
    package_json_copy["version"] = version
    return package_json_copy


def rewrite_versions_of_optional_dependencies(package_json, version):
    package_json_copy = copy.deepcopy(package_json)
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-darwin_x64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-linux_x64"] = version
    package_json_copy["optionalDependencies"]["@zokka/zokka-binary-win32_x64"] = version
    return package_json_copy


try:
    os.environ["NPM_TOKEN"]
except KeyError:
    raise Exception("The NPM_TOKEN environment variable must be set otherwise we cannot publish to npm!")

top_level_npm_directory = "./installers/npm/"
darwin_directory = "./installers/npm/packages/darwin_x64/"
windows_directory = "./installers/npm/packages/win32_x64/"
linux_directory = "./installers/npm/packages/linux_x64/"

try:
    shutil.copyfile(darwin_binary_source_location, darwin_directory + "/zokka")
except shutil.SameFileError:
    print(f"Not copying {darwin_binary_source_location} because location has not changed")
    pass
try:
    shutil.copyfile(windows_binary_source_location, windows_directory + "/zokka.exe")
except shutil.SameFileError:
    print(f"Not copying {windows_binary_source_location} because location has not changed")
    pass
try:
    shutil.copyfile(linux_binary_source_location, linux_directory + "/zokka")
except shutil.SameFileError:
    print(f"Not copying {linux_binary_source_location} because location has not changed")
    pass

for directory in [darwin_directory, windows_directory, linux_directory]:
    with open(directory + "package.json", "r+") as f:
        package_json = json.load(f)
        new_package_json = rewrite_version_of_package_json(package_json, new_version)
        f.seek(0)
        json.dump(new_package_json, f, indent=2)
        f.truncate()
    subprocess.run(["npm", "publish", "--tag", "latest"] + additional_npm_flags, cwd=directory)

with open(top_level_npm_directory + "package.json", "r+") as f:
    package_metadata = json.load(f)
    new_package_metadata = \
        rewrite_version_of_package_json(rewrite_versions_of_optional_dependencies(package_metadata, new_version), new_version)
    f.seek(0)
    json.dump(new_package_metadata, f, indent=2)
    f.truncate()

subprocess.run(["npm", "publish", "--tag", "latest"] + additional_npm_flags, cwd=top_level_npm_directory)
