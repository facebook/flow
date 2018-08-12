#!/bin/bash
# Stop the initialized server
assert_ok "$FLOW" stop;

# Make the file with the sleep visible
mv sleep.js.ignored sleep.js

# Start a new server without waiting
assert_ok "$FLOW" start . \
  --all --no-flowlib \
  --file-watcher "none" \
  --log-file "$FLOW_LOG_FILE" \
  --monitor-log-file "$FLOW_MONITOR_LOG_FILE"

# Make sure retry-if-init exits
assert_exit 7 "$FLOW" status --retry-if-init false
