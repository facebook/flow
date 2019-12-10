# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

if ($env:APPVEYOR_REPO_TAG) {
  pushd "C:\projects\flow\bin"
  mkdir flow
  cp flow.exe flow\flow.exe
  7z a "flow-win64-$env:APPVEYOR_REPO_TAG_NAME.zip" flow
  popd
}

