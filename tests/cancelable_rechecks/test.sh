#!/bin/bash

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
assert_ok "$FLOW" start . \
  --all --no-flowlib --wait \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE"

# Kick off a recheck which will hang for 100s
cp sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js

# This will timeout
assert_exit 3 "$FLOW" status --timeout 5 --no-auto-start

# Removing the file and canceling the recheck will unhang the server
rm sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js
assert_errors "$FLOW" status --no-auto-start --timeout 5
