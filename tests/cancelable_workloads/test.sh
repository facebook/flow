#!/bin/bash

# Workloads can't be canceled anymore by files changing. But in this test
# find-refs starts a recheck which is cancelable. So this test still works.
# So I'm leaving it here.

assert_ok "$FLOW" stop

# Introduce a file which will cause stuff to hang
cp sleep.js.ignored sleep.js

printf "\\n\\nCommands can be canceled\\n"
# Wait for init
start_flow . --lazy

printf "\\n\\nStart a find-refs which will hang:\\n"
"$FLOW" find-refs --global --no-auto-start --strip-root foo.js 3 16 &
COMMAND_PID=$!

sleep 3

printf "  (After sleep find-refs still hasn't output anything)\\n"

printf "\\n\\nTrigger a recheck to fix the hung find-refs\\n"
cp sleep.js.fixed sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js

# assert_ok runs commands in a subshell, but you can't wait from a subshell.
# So we need to manually check that find-refs exited with 0
wait "$COMMAND_PID"
FIND_REFS_RET=$?
if [[ $FIND_REFS_RET != 0 ]]; then
  echo \
    "\`find-refs\` expected to exit code 0 but got $FIND_REFS_RET"
  exit 1
fi

printf "\\n\\nAnd flow status should work\\n"
assert_errors "$FLOW" status --no-auto-start --strip-root
