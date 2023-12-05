#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "" > .flow.saved_state_file_changes

"$FLOW" stop
"$FLOW" start --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state >> /dev/null
"$FLOW" stop
"$FLOW" start --saved-state-fetcher local --wait
"$FLOW" status --strip-root

echo "test.js" > .flow.saved_state_file_changes
echo "" > test.js
"$FLOW" force-recheck test.js --missed-changes --changed-mergebase
assert_errors "$FLOW" status --strip-root
