# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

PATH=/usr/local/bin:/usr/bin:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0
export PATH
OPAMYES=1
export OPAMYES

# print cwd
pwd

# print opam config
opam config list

eval $(opam env)

# print ocaml config
ocamlc -config

cd "${APPVEYOR_BUILD_FOLDER}"
make all
make -C src/parser/ ../../_build/src/parser/test/run_tests.native
