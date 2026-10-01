#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Create a saved state without test.js, then restore the file without reporting
# a file-system change. The lazy server therefore has no ALoc table for the
# file, while check-contents creates a fresh packed signature with keyed
# locations.
mv test.js test.js.hidden
start_flow . --lazy --file-watcher none
assert_ok "$FLOW" save-state --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop . > /dev/null
mv test.js.hidden test.js
: > .flow.saved_state_file_changes
start_flow . \
  --lazy \
  --file-watcher none \
  --saved-state-fetcher local \
  --saved-state-no-fallback

# Disabled assertion-call analysis must not resolve the fresh signature's
# location keys against the missing saved-state table.
assert_ok "$FLOW" check-contents --no-auto-start test.js < test.js
