#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "There should be no errors when the saved state is generated\\n"
assert_ok "$FLOW" status
assert_ok "$FLOW" save-state --root . --out ".flow.saved_state" > /dev/null

assert_ok "$FLOW" stop

cp bar.js.ignored bar.js

echo -e "$(pwd)/bar.js\\n$(pwd)/does_not_exist.php" \
  > ".flow.saved_state_file_changes"

printf "\\nFull init with saved state does recheck & sees new error\\n"
start_flow . --saved-state-fetcher "local" --saved-state-no-fallback
assert_errors "$FLOW" status

assert_ok "$FLOW" stop

printf "\\nLazy init with saved state does NOT recheck & sees 0 errors...\\n"
start_flow . --lazy --saved-state-fetcher "local" --saved-state-no-fallback
assert_ok "$FLOW" status

printf "\\n...but focusing the file exposes the error\\n"
assert_ok "$FLOW" force-recheck --focus --no-auto-start bar.js
assert_errors "$FLOW" status

assert_ok "$FLOW" stop

printf "\\n--saved-state-force-recheck does recheck & sees new error\\n"
start_flow . --lazy --saved-state-fetcher "local" --saved-state-no-fallback \
  --saved-state-force-recheck
assert_errors "$FLOW" status

assert_ok "$FLOW" stop

echo -e "$(pwd)/bar.js\\n$(pwd)/.flowconfig" \
  > ".flow.saved_state_file_changes"

printf "\\nA file incompatible with rechecks changed, so no saved state loading\\n"
# 78 just means flow start failed. The server exited with 20
assert_exit 78 start_flow_unsafe . \
  --saved-state-fetcher "local" --saved-state-no-fallback

printf "\\n...so we need to fallback to non-saved-state\\n"
start_flow . --saved-state-fetcher "local"
assert_errors "$FLOW" status

assert_ok "$FLOW" stop

printf "\\nFallbacks work for lazy mode too\\n"
start_flow . --lazy --saved-state-fetcher "local"
# No errors, since we started in lazy mode so nothing is focused
assert_ok "$FLOW" status
