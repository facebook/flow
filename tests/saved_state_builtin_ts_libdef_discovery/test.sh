#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Generate saved-state"
"$FLOW" stop
"$FLOW" start --saved-state-fetcher none
assert_errors "$FLOW" status --strip-root
"$FLOW" save-state --out new_root/.flow.saved_state >> /dev/null
"$FLOW" stop

echo "" > new_root/.flow.saved_state_file_changes
echo "Use saved state in a different root"
mv .flowconfig lib test.js new_root
cd new_root
"$FLOW" start --lazy --saved-state-fetcher local --saved-state-no-fallback --wait
"$FLOW" force-recheck --focus test.js
assert_errors "$FLOW" status --strip-root
