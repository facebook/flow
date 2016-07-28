if ($env:APPVEYOR_REPO_TAG) {
  pushd "C:\projects\flow\bin"
  mkdir flow
  cp flow.exe flow\flow.exe
  7z a "flow-win64-$env:APPVEYOR_REPO_TAG_NAME.zip" flow
  popd
}

