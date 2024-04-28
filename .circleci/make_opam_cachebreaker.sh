#!/bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

make print-switch
make print-jsoo-version
make print-ounit-version
opam --version
gcc -dumpfullversion -dumpversion
cat flowtype.opam
cat flow_parser.opam
cat .circleci/config.yml
cat .github/workflows/build_and_test.yml
cat .github/actions/install-opam-mac/action.yml
