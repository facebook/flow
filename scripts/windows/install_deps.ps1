# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

choco install --no-progress -y --source https://chocolatey.org/api/v2/ cygwin cyg-get 7zip
if (-not $?) { throw "Failed to install cygwin" }
setx /M PATH $($Env:PATH + ';C:\Program Files\7-Zip')
cyg-get rsync patch diffutils curl make zip unzip git m4 perl mingw64-x86_64-gcc-core mingw64-x86_64-gcc-g++ mingw-w64-x86_64-gcc-libs
if (-not $?) { throw "Failed to install deps from cygwin" }
