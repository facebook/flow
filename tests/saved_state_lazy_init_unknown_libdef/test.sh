#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Lazy init runs a libdef check over every file in `all_unordered_libs`. Those
# files are focused and merged, but the sig dependency graph comes straight out
# of the saved state, so a libdef the saved state never saw has no node in it.
# The merge stream used to look those files up with the panicking `find`, so
# init died with "Node not found in graph".
#
# A libdef dropped into a `[libs]` *directory* reproduces that without touching
# the .flowconfig, so the saved state stays valid. This is the shape www hits,
# where the flowlib prelude and every `[libs]` match are pulled into
# `all_unordered_libs`.

printf "Generate a saved state that predates libs/extra.js\\n"
assert_ok "$FLOW" status
assert_ok "$FLOW" save-state --root . --out ".flow.saved_state" > /dev/null
assert_ok "$FLOW" stop

# The new libdef is deliberately absent from the changed-file list: saved-state
# updates filter out libdefs, so the server never learns about it either way.
cp extra.js.ignored libs/extra.js
echo "$(pwd)/a.js" > ".flow.saved_state_file_changes"

printf "\\nLazy init from that saved state must not crash on the new libdef\\n"
start_flow . --lazy --saved-state-fetcher "local" --saved-state-no-fallback
assert_ok "$FLOW" status

printf "\\nThe new libdef's globals are usable once a file is focused\\n"
cat > b.js <<'EOF'
const y: number = MyOtherGlobal;
const bad: string = MyOtherGlobal;

module.exports = {y, bad};
EOF
assert_ok "$FLOW" force-recheck --focus --no-auto-start b.js
assert_errors "$FLOW" status --strip-root
