#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# S634538: saved state portability for out-of-root files.
# The project root (root/) includes files from outside via [include] ../outside/.
# Save state, then load it at a different location. Without the fix,
# out-of-root File_key suffixes embed machine-specific absolute paths,
# breaking saved state portability.

echo "Generate saved-state"
"$FLOW" stop root
"$FLOW" start root --saved-state-fetcher none
"$FLOW" save-state --root root --out new_location/root/.flow.saved_state >> /dev/null
"$FLOW" stop root

echo "" > new_location/root/.flow.saved_state_file_changes

echo "Use saved state in a different root"
cp root/.flowconfig new_location/root/.flowconfig
cp root/main.js new_location/root/main.js
cp outside/helper.js new_location/outside/helper.js
"$FLOW" start new_location/root --saved-state-fetcher local --wait
assert_errors "$FLOW" status --strip-root new_location/root
