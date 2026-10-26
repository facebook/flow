#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

printf "test.js\n"
assert_ok "$FLOW" agent-helper inspect-types --strip-root test.js

printf "\ntest.js (stdin)\n"
assert_ok "$FLOW" agent-helper inspect-types --strip-root --path test.js < test.js

printf "\ntest.js --show-refinements\n"
assert_ok "$FLOW" agent-helper inspect-types --strip-root --show-refinements test.js
