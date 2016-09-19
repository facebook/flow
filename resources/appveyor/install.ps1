echo "Using ocaml version $env:ocaml_version"
echo "Using ocpwin version $env:win_version"
echo "System architecture: $env:PLATFORM"
echo "Repo build branch is: $env:APPVEYOR_REPO_BRANCH"
echo "Build folder is: $env:APPVEYOR_BUILD_FOLDER"

echo "Downloading $env:ocpwin_uri to $env:ocpwin_zip"
appveyor DownloadFile $env:ocpwin_uri -FileName $env:ocpwin_zip

mkdir ~/AppData/Roaming/OCamlPro
mkdir ~/AppData/Roaming/OCamlPro/OCPWin

echo "Installing ocpwin..."
pushd ~/AppData/Roaming/OCamlPro/OCPWin
try {
  7z x $env:ocpwin_zip
  ls
  cd $env:ocpwin_version

  ./bin/ocpwin -in 2>&1 | %{ "$_" }
  if ($LASTEXITCODE -gt 0) {
    Throw "The ocpwin installation exited with error code: $LASTEXITCODE"
  }

  # Verify we have actually installed the right version of ocpwin
  $installed_versions = ./bin/ocpwin -list
  echo $installed_versions
  if (-Not ($installed_versions | grep $env:ocpwin_version)) {
    Throw "ocpwin appeared to suceed but ocpwin -list is missing $env:ocpwin_version"
  }
} finally {
  popd
}

# ocpwin modifies the path, so we need to reload it for ocaml utils to be available
echo "Reloading the path..."
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")

$installed_ocaml_version = ocaml -version
echo $installed_ocaml_version
if (-Not ($installed_ocaml_version | grep $env:ocaml_version)) {
  Throw "ocaml -version is not some form of $env:ocaml_verson"
}

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
