#!/bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

make print-switch
make print-jsoo-version
opam --version
gcc -dumpfullversion -dumpversion
cat flowtype.opam
cat flow_parser.opam
cat .circleci/config.yml
