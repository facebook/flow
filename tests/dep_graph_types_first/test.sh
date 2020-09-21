#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

printf "\\n\\nStart server in types-first mode\\n"
start_flow .

printf "\\nValue dep-graph should include both edges\\n"
assert_ok "$FLOW" graph dep-graph --strip-root --out types-first-values.log
cat types-first-values.log

printf "\\n\\nType dep-graph should include one edge\\n"
assert_ok "$FLOW" graph dep-graph --strip-root --types --out types-first-types.log
cat types-first-types.log

assert_ok "$FLOW" stop

printf "\\n"
