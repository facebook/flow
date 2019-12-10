#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

rm .flowconfig
assert_ok "$FLOW" init

printf "Default .flowconfig should typecheck:\n"
assert_ok "$FLOW" check

printf "\nDefault .flowconfig looks like this:\n"
cat .flowconfig
