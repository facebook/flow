#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

cp lib/lib.js lib/lib.js.orig

# This should not cause the Flow server to die
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js
echo "first status, after recheck"
assert_errors "$FLOW" status --no-auto-start 2>/dev/null

# This also should not cause the Flow server to die
touch lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js
echo "second status, after touch"
assert_errors "$FLOW" status --no-auto-start 2>/dev/null

# This also should not cause the Flow server to die, and Flow should return full errors.
cp lib/lib.js.modified lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js
echo "third status, after modification"
# Back before FlowServerMonitor, the server would die. Now the monitor will
# start one back up
assert_errors "$FLOW" status --no-auto-start 2>/dev/null

assert_ok "$FLOW" stop
# Start the server back up but turn off the restarting behavior
start_flow . --no-auto-restart

# This also should not cause the Flow server to die, and Flow should return full errors.
cp lib/lib.js.orig lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js

echo "fourth status, after modification"
# This also should not cause the Flow server to die, and Flow should return full errors.
assert_errors "$FLOW" status --no-auto-start 2>/dev/null

# This should fixed the type error
cp lib/lib.js.fixed lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js

echo "fixth status, after modification"
# All flow errors should be fixed.
assert_ok "$FLOW" status --no-auto-start 2>/dev/null

echo "done"

mv lib/lib.js.orig lib/lib.js
