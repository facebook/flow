#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nServer should start in fs lazy mode and in types-first mode\\n"
start_flow . --lazy

assert_ok "$FLOW" status --strip-root

printf "\\nWarm up cyclic dependencies\\n"
assert_ok "$FLOW" force-recheck --focus types.js
assert_ok "$FLOW" status --strip-root

printf "\\nEditing a file should cause dependents to pull in dependencies in the same cycle\\n"
assert_ok "$FLOW" force-recheck --focus touched.js
assert_ok "$FLOW" status --strip-root

assert_ok "$FLOW" stop
