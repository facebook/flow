#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

printf "\\nInit cannot be canceled\\n"
# Add a file which sleeps for 100s
cp sleep.js.ignored sleep.js

# Don't wait for init - this server is going to hang
assert_ok "$FLOW" start . \
  --all --no-flowlib \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE"

# This will timeout
assert_exit 3 \
  "$FLOW" force-recheck sleep.js --profile --timeout 5 --no-auto-start

rm sleep.js

assert_ok "$FLOW" stop

printf "\\n\\nRecheck can be canceled\\n"
# Wait for init
start_flow .

# Kick off a recheck which will hang for 100s
cp sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js

# This will timeout
assert_exit 3 "$FLOW" status --timeout 5 --no-auto-start

# If we change sleep_dependent.js, the recheck will cancel. If we for some
# reason fail to rollback the transaction, sleep.js will be missing from the
# shared memory, so typechecking sleep_dependent.js will produce
#
# Internal error: uncaught exception: Utils_js.Key_not_found("LeaderHeap", ".../cancelable_rechecks/sleep.js")
#
# So this is intended to test that we ARE rolling back the transaction properly
echo " " >> sleep_dependent.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep_dependent.js

# This will timeout
assert_exit 3 "$FLOW" status --timeout 5 --no-auto-start

# Removing the file and canceling the recheck will unhang the server
rm sleep.js sleep_dependent.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js
assert_errors "$FLOW" status --no-auto-start --timeout 5
