# Copyright (c) Meta Platforms, Inc. and affiliates.
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

winget install opam --disable-interactivity --accept-source-agreements --location $cygwin_root
