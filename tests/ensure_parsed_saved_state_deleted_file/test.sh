#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "==== No errors at start====\\n"
assert_ok "$FLOW" status --no-auto-start

rm A.js

# Simulate a file watcher race condition. Let's say B.js becomes focused, but the file watcher has not yet noticed that A.js was removed.
assert_ok "$FLOW" force-recheck --focus B.js

printf "\\n\\n==== ensure_parsed notices that A was removed ====\\n"
# Focusing on A.js requires B.js to be checked, so ensure_parsed tries to parse
# it. But it notices it has been removed and triggers a recheck, which finds the
# error
echo "NOTE: This error message has been modified to remove the full file path."
# For some reason, the full path of `A.js` appears here, even with
# `--strip-root`. I don't have time to dig into it right now, so I'm just using
# `sed` to remove it so that it doesn't perturb the test output.
assert_errors "$FLOW" status --no-auto-start --strip-root | sed -e 's/`[^`]*A.js`/`A.js`/'
