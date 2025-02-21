#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Generate saved-state"
"$FLOW" stop
"$FLOW" start --saved-state-fetcher none
"$FLOW" save-state --out new_root/.flow.saved_state >> /dev/null
"$FLOW" stop

echo "" > new_root/.flow.saved_state_file_changes
echo "Use saved state in a different root"
mv .flowconfig new_root
cd new_root
"$FLOW" start --saved-state-fetcher local --wait
echo "const a: string = new Object()" > test.js
"$FLOW" force-recheck test.js
assert_errors "$FLOW" status --strip-root
