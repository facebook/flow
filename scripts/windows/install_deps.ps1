# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

choco install --no-progress -y --source https://chocolatey.org/api/v2/ cygwin cyg-get 7zip
if (-not $?) { throw "Failed to install cygwin" }
setx /M PATH $($Env:PATH + ';C:\Program Files\7-Zip')
cyg-get rsync patch diffutils curl make zip unzip git m4 perl mingw64-x86_64-gcc-core mingw64-x86_64-gcc-g++ mingw-w64-x86_64-gcc-libs
if (-not $?) { throw "Failed to install deps from cygwin" }

# Ocaml < 4.12 doesn't work with binutils 2.36 and cygwin doesn't provide a legit way
# to install older packages via CLI. We download the old version and extract it on top,
# which is a hack because cygwin doesn't know we did it. :(
$local_packages = "$Env:TEMP\flow\cygwin"
New-Item -ItemType Directory $local_packages
(New-Object System.Net.WebClient).DownloadFile("https://mirrors.kernel.org/sourceware/cygwin/x86_64/release/mingw64-x86_64-binutils/mingw64-x86_64-binutils-2.35.2-1.tar.xz", "$local_packages\mingw64-x86_64-binutils-2.35.2-1.tar.xz")
if (-not $?) { throw "Failed to download mingw64-x86_64-binutils-2.35.2-1" }
7z.exe x "$local_packages\mingw64-x86_64-binutils-2.35.2-1.tar.xz" -o"$local_packages" -y
7z.exe x "$local_packages\mingw64-x86_64-binutils-2.35.2-1.tar" -o"C:\tools\cygwin" -y
