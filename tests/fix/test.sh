#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "flow codemod fix-errors\n"
assert_ok "$FLOW" codemod fix-errors --strip-root
printf "flow codemod fix-errors --error-codes method-unbinding\n"
assert_ok "$FLOW" codemod fix-errors --error-codes method-unbinding --strip-root
