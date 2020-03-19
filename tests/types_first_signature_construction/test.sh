#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Run with classic
assert_errors "$FLOW" check --show-all-errors --strip-root > classic-errors.log
cat classic-errors.log

# Run with types-first
assert_errors "$FLOW" check --types-first --show-all-errors --strip-root > types-first-errors.log

assert_one() {
  assert_exit_on_line "${BASH_LINENO[0]}" "1" "$@"
}
printf "==== DIFF BETWEEN CLASSIC AND TYPES-FIRST =====\n"
assert_ok diff classic-errors.log types-first-errors.log > diff.log
cat diff.log
