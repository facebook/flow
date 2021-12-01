#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
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

# This should cause the flow server to die, but the monitor will restart one
cp lib/lib.js.modified lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js
echo "third status, after modification"
# Back before FlowServerMonitor, the server would die. Now the monitor will
# start one back up
assert_errors "$FLOW" status --no-auto-start 2>/dev/null

assert_ok "$FLOW" stop
# Start the server back up but turn off the restarting behavior
start_flow . --no-auto-restart

# This should cause the flow server and monitor to die, due to --no-auto-restart
cp lib/lib.js.orig lib/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start lib/lib.js

echo "fourth status, after modification"
# This should have no output, since it won't find a server. It will print stuff
# on stderr but it includes the nondeterministically-chosen tmpdir so we can't
# compare against it.
assert_exit 6 "$FLOW" status --no-auto-start 2>/dev/null

echo "done"

mv lib/lib.js.orig lib/lib.js
