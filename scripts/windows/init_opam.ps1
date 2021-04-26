# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

$cygwin_root = (Get-ItemProperty 'HKLM:\SOFTWARE\Cygwin\setup' -ea 0).rootdir
if (!$cygwin_root) {
    echo "Cygwin not found!"
    exit
}

& $cygwin_root\bin\bash.exe -l "$PSScriptRoot\init_opam.sh"
