#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# --saved-state-no-fallback should mean that Flow gives up if it is unable to
# find a saved state file.
# Exit code 78 indicates that server start failed (start_flow_unsafe adds the
# --wait flag).
assert_exit 78 start_flow_unsafe . --saved-state-fetcher "local" --saved-state-no-fallback

assert_ok "$FLOW" stop

# If there is an invalid saved state file, --saved-state-no-fallback makes Flow
# give up.
echo "definitely not a real saved state file" > .flow.saved_state
echo "" > .flow.saved_state_file_changes
assert_exit 78 start_flow_unsafe . --saved-state-fetcher "local" --saved-state-no-fallback

assert_ok "$FLOW" stop
