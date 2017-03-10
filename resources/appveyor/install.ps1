echo "System architecture: $env:PLATFORM"
echo "Repo build branch is: $env:APPVEYOR_REPO_BRANCH"
echo "Build folder is: $env:APPVEYOR_BUILD_FOLDER"

echo "Installing npm dependencies..."
pushd $PSScriptRoot\..\..
try {
  npm install 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "npm install exited with the error code: $LASTEXITCODE"
  }
} finally {
  popd
}
