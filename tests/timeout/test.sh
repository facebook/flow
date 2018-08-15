#!/bin/bash

# Only make sleep.js visible after we've started the server. Otherwise, we'll
# have to wait like 10s before we even start running test.sh
mv sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck sleep.js

assert_exit 3 "$FLOW" status --timeout 1
assert_exit 3 "$FLOW" get-def --timeout 1 sleep.js 1 1 < sleep.js
