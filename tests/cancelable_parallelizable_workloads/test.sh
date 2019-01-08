#!/bin/bash

assert_ok "$FLOW" stop

printf "\\n\\nCommands can be canceled\\n"
# Wait for init
start_flow . --lazy

printf "\\n\\nServer starts off checking nothing\\n"
assert_ok "$FLOW" status --strip-root --no-auto-start

printf "\\n\\ncheck-contents triggers a recheck due to unchecked dependency:\\n"
assert_errors "$FLOW" check-contents --strip-root --no-auto-start focused.js < focused.js

printf "\\n\\nTrigger a 1000s recheck\\n"
mv sleep.js.ignored dependency.js
assert_ok "$FLOW" force-recheck dependency.js

printf "\\n\\nProve we're running in parallel by running a command:\\n"
assert_errors "$FLOW" check-contents --strip-root --no-auto-start --timeout 30 focused.js < focused.js

printf "\\n\\nStop the stuck recheck\\n"
assert_ok "$FLOW" stop
