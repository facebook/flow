#!/bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "ocaml-$1"
opam --version
gcc -dumpfullversion -dumpversion
cat flowtype.opam
cat flow_parser.opam
cat .circleci/config.yml
