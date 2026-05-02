#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that verify_flowconfig_hash during saved-state reinit compares against
# the current on-disk .flowconfig rather than the stale startup-time hash.
#
# 1. Create saved state A with original .flowconfig (hash A)
# 2. Create saved state B with modified .flowconfig (hash B — just a comment)
# 3. Start server loading saved state A (options.flowconfig_hash = hash A)
# 4. Swap .flowconfig to B on disk and .flow.saved_state to B
# 5. force-recheck --missed-changes --changed-mergebase triggers reinit,
#    which loads saved state B
#
# At step 5, verify_flowconfig_hash compares saved state hash (B) against the
# flowconfig. Before the fix it used the stale startup hash (A) — mismatch,
# server dies. After the fix it reads from disk (B) — match, reinit succeeds.

# Save original .flowconfig
cp .flowconfig .flowconfig.orig

# Phase 1: Create saved state with original config (A)
echo "" > .flow.saved_state_file_changes
"$FLOW" stop
"$FLOW" start --file-watcher=none --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state_a >> /dev/null
"$FLOW" stop

# Phase 2: Add a comment to change the hash, create saved state (B)
echo "# comment to change flowconfig hash" >> .flowconfig
"$FLOW" start --file-watcher=none --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state_b >> /dev/null
"$FLOW" stop

# Phase 3: Restore config A and start server loading saved state A
cp .flowconfig.orig .flowconfig
cp .flow.saved_state_a .flow.saved_state
"$FLOW" start --file-watcher=none --saved-state-fetcher local --wait

printf "Initial status (should have no errors):\n"
assert_ok "$FLOW" status --strip-root

# Phase 4: Swap to config B + saved state B, trigger reinit
cp .flowconfig.orig .flowconfig
echo "# comment to change flowconfig hash" >> .flowconfig
cp .flow.saved_state_b .flow.saved_state
echo "test.js" > .flow.saved_state_file_changes
cat > test.js <<'EOF'
// @flow

import dep from './dep';
dep as string;
EOF
"$FLOW" force-recheck test.js --missed-changes --changed-mergebase
"$FLOW" force-recheck --no-auto-start --focus test.js

printf "After reinit (should have type error):\n"
assert_errors "$FLOW" status --strip-root --no-auto-start
