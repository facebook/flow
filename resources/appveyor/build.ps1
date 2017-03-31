pushd $PSScriptRoot\..\..

$env:PATH += ";C:\cygwin64\home\appveyor\.opam\$env:OPAM_SWITCH\bin"
$env:OCAMLLIB = "C:\cygwin64\home\appveyor\.opam\$env:OPAM_SWITCH\lib"

try {
  ./make.bat 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "make.bat exited with error code: $LASTEXITCODE"
  }
} finally {
  popd
}
