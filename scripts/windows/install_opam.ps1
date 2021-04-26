# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

$ErrorActionPreference = "Stop"

$cygwin_root = (Get-ItemProperty 'HKLM:\SOFTWARE\Cygwin\setup' -ea 0).rootdir
if (!$cygwin_root) {
    echo "Cygwin not found!"
    exit
} else {
    echo "Found cygwin in $cygwin_root"
}

$install_dir = "$Env:TEMP\flow\opam_installer"
New-Item -ErrorAction Ignore -ItemType Directory $install_dir
echo "Downloading opam64.tar.xz to $install_dir\opam64.tar.xz"
(New-Object System.Net.WebClient).DownloadFile("https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz", "$install_dir\opam64.tar.xz")
$out_dir = "$install_dir".Replace("\", "/")
echo "Extracting opam64.tar.xz"
& $cygwin_root\bin\bash.exe -l -c "tar -x --force-local -f '$install_dir\opam64.tar.xz' -C '$out_dir'"
echo "Installing opam"
& $cygwin_root\bin\bash.exe -l "$install_dir\opam64\install.sh"
echo "Done"
