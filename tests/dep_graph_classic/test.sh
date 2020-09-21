#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

printf "\\nStart server in classic mode\\n"
assert_ok "$FLOW" start .

printf "\\nValue dep-graph should include both edges\\n"
assert_ok "$FLOW" graph dep-graph --strip-root --out classic-values.log
cat classic-values.log

printf "\\n\\nType dep-graph should include both edges\\n"
assert_ok "$FLOW" graph dep-graph --strip-root --types --out classic-types.log
cat classic-types.log

assert_ok "$FLOW" stop
printf "\\n"
