pushd $PSScriptRoot\..\..

try {
  ./make.bat 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "make.bat exited with error code: $LASTEXITCODE"
  }
} finally {
  popd
}