#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "There should be no errors when the saved state is generated\\n"
assert_ok "$FLOW" status
assert_ok "$FLOW" save-state --root . --out ".flow.saved_state" > /dev/null

assert_ok "$FLOW" stop

mv A.js.ignored A.js
mv B.js.ignored B.js

# Deliberately only tell Flow that A changed. It will discover that B changed in
# ensure_parsed when we go to update the dependency graph.
echo "$(pwd)/A.js" > ".flow.saved_state_file_changes"

# Flow notices that B has also changed, so it reparses B too in order to update
# the dependency graph, but does not trigger a recheck.
printf "\\nLazy init with saved state does not trigger a recheck\\n"
start_flow . --lazy --saved-state-fetcher "local" --saved-state-no-fallback
assert_ok "$FLOW" status

printf "\\nFocusing A.js reveals the errors\\n"
"$FLOW" force-recheck --focus --no-auto-start A.js
assert_errors "$FLOW" status

assert_ok "$FLOW" stop
