#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that when a libdef changes during a mergebase change, we try
# saved-state reinit first, and fall back to reinit_full_check if the
# saved state doesn't match the current lib on disk.

######################################################################
# Setup: create two saved states with different lib contents
######################################################################

# Saved state A: lib declares myGlobal: string
echo "" > .flow.saved_state_file_changes
"$FLOW" stop 2>/dev/null
start_flow . --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state_A >> /dev/null
"$FLOW" stop

# Change lib and create saved state B: lib declares myGlobal: number
cat > libs/lib.js <<'LIBEOF'
// @flow
declare var myGlobal: number;
LIBEOF
echo "" > .flow.saved_state_file_changes
start_flow . --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state_B >> /dev/null
"$FLOW" stop

######################################################################
# Scenario 1: upstream-only libdef change
######################################################################

# Restore old lib (state A) and start server from saved state A
cat > libs/lib.js <<'LIBEOF'
// @flow
declare var myGlobal: string;
LIBEOF
cp .flow.saved_state_A .flow.saved_state
echo "" > .flow.saved_state_file_changes
start_flow . --saved-state-fetcher local

echo "Scenario 1: upstream libdef change"
echo "Before recheck:"
assert_ok "$FLOW" status --strip-root

# Now simulate mergebase change: update lib on disk to match saved state B
cat > libs/lib.js <<'LIBEOF'
// @flow
declare var myGlobal: number;
LIBEOF
# Point to saved state B for reinit, with no additional file changes
cp .flow.saved_state_B .flow.saved_state
echo "" > .flow.saved_state_file_changes

# Trigger reinit with changed mergebase + lib change
assert_ok "$FLOW" force-recheck libs/lib.js --changed-mergebase --missed-changes

# Wait for recheck to complete and show status
echo "After recheck:"
"$FLOW" status --strip-root 2>/dev/null || true

# Check which reinit path was used
echo "Used saved-state reinit:"
grep -q "Reinitializing from saved state" "$FLOW_LOG_FILE" && echo "yes" || echo "no"
echo "Did NOT fall back to reinit_full_check:"
grep -q "Saved-state reinit failed; falling back to reinit_full_check" "$FLOW_LOG_FILE" && echo "no" || echo "yes"
echo ""

"$FLOW" stop

######################################################################
# Scenario 2: local libdef change
######################################################################

# Restore old lib (state A) and start server from saved state A
cat > libs/lib.js <<'LIBEOF'
// @flow
declare var myGlobal: string;
LIBEOF
cp .flow.saved_state_A .flow.saved_state
echo "" > .flow.saved_state_file_changes
start_flow . --saved-state-fetcher local

echo "Scenario 2: local libdef change"
echo "Before recheck:"
assert_ok "$FLOW" status --strip-root

# Simulate mergebase change + local lib modification:
# lib on disk differs from both saved state A and B
cat > libs/lib.js <<'LIBEOF'
// @flow
declare var myGlobal: boolean;
LIBEOF
# Point to saved state B for reinit attempt (but lib on disk is boolean, not number)
cp .flow.saved_state_B .flow.saved_state
echo "libs/lib.js" > .flow.saved_state_file_changes

assert_ok "$FLOW" force-recheck libs/lib.js --changed-mergebase --missed-changes

# Wait for recheck to complete and show status
echo "After recheck:"
"$FLOW" status --strip-root 2>/dev/null || true

# Check which reinit path was used
echo "Tried saved-state reinit:"
grep -q "Libdef changed with mergebase change; trying saved-state reinit first" "$FLOW_LOG_FILE" && echo "yes" || echo "no"
echo "Fell back to reinit_full_check:"
grep -q "Saved-state reinit failed; falling back to reinit_full_check" "$FLOW_LOG_FILE" && echo "yes" || echo "no"
echo ""
