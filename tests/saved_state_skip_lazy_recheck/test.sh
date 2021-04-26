#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp test1.js tmp/

printf "\\nServer should start in types-first mode\\n"
start_flow . --saved-state-fetcher local --saved-state-no-fallback --lazy-mode fs

printf "\\nServer should have no errors because it's lazy\\n"
assert_ok "$FLOW" status --strip-root --no-auto-start

printf "\\nAdding a line should cause minimal rechecking and expose the error from test1.js only\\n"
cp tmp1/test1.js test1.js
assert_ok "$FLOW" force-recheck --focus test1.js --no-auto-start
assert_errors "$FLOW" status --strip-root --no-auto-start
show_skipping_stats_types_first "$FLOW_LOG_FILE"

printf "\\nChanging the type of the export should expose the error in test2.js\\n"
cp tmp2/test1.js test1.js
assert_ok "$FLOW" force-recheck --focus test1.js --no-auto-start
assert_errors "$FLOW" status --strip-root --no-auto-start
show_skipping_stats_types_first "$FLOW_LOG_FILE"

printf "\\nChanging the exported type should expose the error in test3.js\\n"
cp tmp3/test1.js test1.js
assert_ok "$FLOW" force-recheck --focus test1.js --no-auto-start
assert_errors "$FLOW" status --strip-root --no-auto-start
show_skipping_stats_types_first "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop

rm -rf tmp
