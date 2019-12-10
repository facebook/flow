# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "System architecture: $env:PLATFORM"
echo "Repo build branch is: $env:APPVEYOR_REPO_BRANCH"
echo "Build folder is: $env:APPVEYOR_BUILD_FOLDER"

echo "Installing npm dependencies..."
pushd $PSScriptRoot\..\..
try {
  yarn install --ignore-scripts --pure-lockfile 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "yarn install exited with the error code: $LASTEXITCODE"
  }
} finally {
  popd
}
