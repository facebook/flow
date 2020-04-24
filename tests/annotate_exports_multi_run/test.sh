#! /bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

assert_ok "$FLOW" codemod annotate-exports --strip-root --repeat ./*.js 2> /dev/null

printf "\n=== codemodded files ===\n\n"

tail -n +1 ./*.js
