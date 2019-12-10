#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This test is primarily intended to test that Types_js.ensure_parsed properly
# parses things which were not originally parsed due to saved_state

printf "==== No errors at start====\\n"
assert_ok "$FLOW" status --no-auto-start

cp B.js B.js.orig
cp C.js C.js.orig

# Simulate a file watcher race condition. Let's say it notices A.js changed
# but not that B.js changed
cp B.js.with_parse_error B.js
assert_ok "$FLOW" force-recheck --focus A.js

printf "\\n\\n==== ensure_parsed notices that B has changed ====\\n"
# Focusing on A.js requires B.js to be checked, so ensure_parsed tries to
# parse it. But it notices it has changed and triggers a recheck, which finds
# the parse errors
assert_errors "$FLOW" status --no-auto-start

# Reset the parse error
cp B.js.orig B.js
assert_ok "$FLOW" stop
start_flow . --lazy-mode ide

printf "==== No errors after restart====\\n"
assert_ok "$FLOW" status --no-auto-start

cp C.js.with_new_dependency C.js
cp A.js.modified A.js
# Tell the server that A.js has changed but don't tell it about C
assert_ok "$FLOW" force-recheck --focus A.js

printf "\\n\\n==== ensure_parsed notices that C has changed ====\\n"
# Focusing on A.js requires C.js to be checked, so ensure_parsed tries to
# parse it. But it notices it has changed and triggers a recheck, which finds
# the error
assert_errors "$FLOW" status --no-auto-start
