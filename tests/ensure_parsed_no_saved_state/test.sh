#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This test is intended to complement the ensure_parsed_saved_state test and
# demonstrate the different behavior of ensure_parsed when saved state is not
# used
printf "==== No errors at start====\\n"
assert_ok "$FLOW" status --no-auto-start

cp B.js B.js.orig
cp C.js C.js.orig

# Simulate a file watcher race condition. Let's say it notices A.js changed
# but not that B.js changed
cp B.js.with_parse_error B.js
assert_ok "$FLOW" force-recheck --focus A.js

printf "\\n\\n==== Still don't notice the error ====\\n"
# The parse error should not be noticed yet. The server does recheck B.js, but
# it will use the AST stored in shared memory. If we started up with saved state
# we wouldn't have the AST in memory and would notice the error when we try to
# read B.js from disk and parse it
assert_ok "$FLOW" status --no-auto-start

assert_ok "$FLOW" force-recheck B.js

printf "\\n\\n==== Now we see the parse error ====\\n"
# The parse error should now be noticed
assert_errors "$FLOW" status --no-auto-start


# Reset the parse error
cp B.js.orig B.js
cp C.js.with_new_dependency C.js
assert_ok "$FLOW" force-recheck A.js B.js # But not C.js

printf "\\n\\n==== We don't see the unknown module exception yet ====\\n"
# The error should not be noticed yet. The server does recheck C.js, but
# it will use the AST stored in shared memory. If we started up with saved state
# we wouldn't have the AST in memory and would notice the error when we try to
# read C.js from disk, parse it, and recheck the changed file
assert_ok "$FLOW" status --no-auto-start

assert_ok "$FLOW" force-recheck C.js

printf "\\n\\n==== Now we see the unknown module error ====\\n"
# The error should now be noticed
assert_errors "$FLOW" status --no-auto-start
