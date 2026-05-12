#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Save original package.json
cp node_modules/pkg/package.json node_modules/pkg/package.json.orig

printf "\nState 1: types -> a.d.ts (x is number)\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nState 2: types -> b.d.ts (x is string)\n"
cp tmp/package.json node_modules/pkg/package.json
# Server restarts on incompatible package.json change, so force-recheck may or
# may not return successfully depending on whether the response reaches the
# client before the server exits. Ignore the exit code.
"$FLOW" force-recheck --no-auto-start node_modules/pkg/package.json || true

# Wait for the server to restart, then stop it and start a non-lazy one
# so that all files get checked.
sleep 2
"$FLOW" stop . 2>/dev/null || true
sleep 1
assert_ok "$FLOW" start --file-watcher none --lazy-mode none --wait .
assert_errors "$FLOW" status --no-auto-start .

# Restore original
mv node_modules/pkg/package.json.orig node_modules/pkg/package.json
