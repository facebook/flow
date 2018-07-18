#!/bin/bash
. ../assert.sh
FLOW=$1

"$FLOW" stop

assert_ok "$FLOW" start . \
  --all --no-flowlib --wait \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE" \
  --lazy-mode ide

printf "===== (ide) force-recheck of a unchanged file does nothing: =====\\n"
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start

printf "\\n===== (ide) force-recheck --focus of a unchanged file does a recheck: =====\\n"
assert_ok "$FLOW" force-recheck --focus a.js
assert_errors "$FLOW" status --no-auto-start

"$FLOW" stop

assert_ok "$FLOW" start . \
  --all --no-flowlib --wait \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE" \
  --lazy-mode fs

printf "===== (fs) force-recheck of a unchanged file does nothing: =====\\n"
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start

printf "\\n===== (fs) force-recheck --focus of a unchanged file does a recheck: =====\\n"
assert_ok "$FLOW" force-recheck --focus a.js
assert_errors "$FLOW" status --no-auto-start
