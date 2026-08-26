#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# A saved state is deserialized into the committed heap *before* it is
# validated. When validation then rejects it and Flow falls back to a full
# init, entries for files that only exist in the rejected saved state must not
# survive -- the fallback init only clears the reader cache, not the heap
# writes that `flow_saved_state::load` already made.
#
# Here stale/dupe.js provides haste module `dupe` when the saved state is
# written, and is moved to live/dupe.js afterwards. A newly added package.json
# invalidates the saved state, but only after its heap has been loaded, so the
# fallback init runs with stale/dupe.js still registered as a haste provider
# candidate. It then collides with the real provider live/dupe.js.

: > .flow.saved_state_file_changes

"$FLOW" start --saved-state-fetcher none --wait
"$FLOW" save-state --out .flow.saved_state >> /dev/null
"$FLOW" stop

# Move the haste module provider out from under the saved state. `live` sorts
# before `stale`, so the on-disk file is the chosen provider and the stale heap
# entry shows up as the duplicate.
mkdir -p live
mv stale/dupe.js live/dupe.js

# Add a package, which is what makes the saved state invalid.
mkdir -p pkg
echo '{"name":"pkg","main":"main.js"}' > pkg/package.json
printf '// @flow\nmodule.exports = 1;\n' > pkg/main.js

printf 'stale/dupe.js\nlive/dupe.js\npkg/package.json\npkg/main.js\n' \
  > .flow.saved_state_file_changes

printf "start after the saved state is rejected (expected: server starts):\n"
start_flow_unsafe . --saved-state-fetcher local
start_exit=$?
printf "exit code: %s\n" "$start_exit"
echo

# Guarded: if the server failed to start, `flow status` would silently
# autostart a second, clean server and print "No errors!", hiding the failure
# above.
if [ "$start_exit" -eq 0 ]; then
  printf "status (expected: no errors):\n"
  status_exit=0
  "$FLOW" status --strip-root || status_exit=$?
  printf "exit code: %s\n" "$status_exit"
  "$FLOW" stop
fi
