#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that the server restarts (rather than dies) when .flowconfig or
# package.json changes. Before this fix, these changes caused
# Exit.Flowconfig_changed (exit 16), which killed both the server and
# monitor. After the fix, they exit with Exit.Restart (exit 7), so the
# monitor restarts the server.
#
# The status checks after changes use --no-auto-start: if the monitor
# died, this fails. If the monitor restarted the server, it succeeds.

echo "Before any changes:"
assert_errors "$FLOW" status --no-auto-start .

# --- Test 1: .flowconfig hash change ---
cp .flowconfig .flowconfig.orig
echo "; this comment changes the hash" >> .flowconfig

"$FLOW" force-recheck --no-auto-start .flowconfig 2>/dev/null || true
sleep 2

echo ""
echo "After .flowconfig change (server should have restarted):"
assert_errors "$FLOW" status --no-auto-start . 2>/dev/null

mv .flowconfig.orig .flowconfig

# --- Test 2: package.json incompatible change ---
mkdir -p node_modules/pkg
echo '{"name": "pkg", "main": "index.js"}' > node_modules/pkg/package.json

"$FLOW" force-recheck --no-auto-start node_modules/pkg/package.json 2>/dev/null || true
sleep 2

echo ""
echo "After package.json change (server should have restarted):"
assert_errors "$FLOW" status --no-auto-start . 2>/dev/null

rm -rf node_modules
