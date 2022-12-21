#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This test is primarily intended to test that Types_js.ensure_parsed properly
# parses things which were not originally parsed due to saved_state

printf "==== No errors at start====\\n"
assert_ok "$FLOW" status --no-auto-start

# focusing file.js means its dependency, dependency.js, needs to be parsed
# when using lazy mode + saved state.
assert_ok "$FLOW" force-recheck --focus file.js

printf "\\n\\n==== ensure_parsed does not recheck unrelated dependent ====\\n"
# ensure_parsed included dependency.js, but does not fan out to dependency.js's
# dependents (dependent.js).
assert_ok "$FLOW" status --no-auto-start

assert_ok "$FLOW" stop
start_flow .

# modify dependency.js from what's in saved state, but don't tell the server
# about it (an unexpected change). this simulates a race condition between
# the file watcher and the server.
printf "\nexport const bar = 123;\n" >> dependency.js

printf "==== No errors after restart====\\n"
assert_ok "$FLOW" status --no-auto-start

# focusing file.js means its dependency, dependency.js, needs to be parsed
# when using lazy mode + saved state.
assert_ok "$FLOW" force-recheck --focus file.js

printf "\\n\\n==== ensure_parsed skips dependent ====\\n"
# ensure_parsed included dependency.js as a dependency update, which means
# it does not fan out to dependency.js's dependents (dependent.js)
assert_ok "$FLOW" status --no-auto-start
show_skipping_stats "$FLOW_LOG_FILE"

# now simulate the file system event for dependency.js
assert_ok "$FLOW" force-recheck dependency.js

printf "\\n\\n==== force-recheck checks dependent ====\\n"
# even though we've already partially processed the change to dependency.js
# (updated the parsing heaps and the dep graph), its signature changed so we
# need to recheck its dependents now.
assert_errors "$FLOW" status --no-auto-start
show_skipping_stats "$FLOW_LOG_FILE"

# now focus dependent.js (similar to opening it in an IDE)
assert_ok "$FLOW" force-recheck --focus dependent.js

printf "\\n\\n==== force-recheck on dependent finds errors ====\\n"
assert_errors "$FLOW" status --no-auto-start
# should check dependent.js (0 of 1 skipped)
show_skipping_stats "$FLOW_LOG_FILE"
