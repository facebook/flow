#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that saved state save/load works with parallel decompress enabled.
# The env-first file format and parallel heap load + env decompress should
# produce the same results as sequential loading.
#
# 1. Create a saved state with direct serialization + parallel decompress
# 2. Load from that saved state
# 3. Trigger reinit via --changed-mergebase
# 4. Verify errors are correctly reported after reinit

echo "" > .flow.saved_state_file_changes

"$FLOW" stop
start_flow . --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state >> /dev/null
"$FLOW" stop
start_flow . --saved-state-fetcher local

printf "Initial status (should have no errors):\n"
assert_ok "$FLOW" status --strip-root

# Trigger reinit: mark test.js as changed and introduce a type error
echo "test.js" > .flow.saved_state_file_changes
cat > test.js <<'EOF'
// @flow

import dep from './dep';
(dep: string);
EOF
"$FLOW" force-recheck test.js --missed-changes --changed-mergebase

printf "After reinit (should have type error):\n"
assert_errors "$FLOW" status --strip-root
