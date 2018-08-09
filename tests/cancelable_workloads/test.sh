#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" stop

# Introduce a file which will cause stuff to hang
cp sleep.js.ignored sleep.js

printf "\\n\\nCommands can be canceled\\n"
# Wait for init
assert_ok "$FLOW" start . \
  --all --no-flowlib --wait --lazy \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE"

printf "\\n\\nStart a find-refs which will hang:\\n"
"$FLOW" find-refs --global --no-auto-start --strip-root foo.js 3 16 &
COMMAND_PID=$!

sleep 3

printf "  (After sleep find-refs still hasn't output anything)\\n"

printf "\\n\\nTrigger a recheck to fix the hung find-refs\\n"
cp sleep.js.fixed sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js

assert_ok wait "$COMMAND_PID"

printf "\\n\\nAnd flow status should work\\n"
assert_errors "$FLOW" status --no-auto-start --strip-root
