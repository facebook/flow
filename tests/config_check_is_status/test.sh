#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# With check_is_status=true, `flow check` should connect to the server
# (like `flow status`) instead of doing a standalone full-check.
assert_errors "$FLOW" check . --strip-root --show-all-errors

# Verify a server was started (only happens in status mode, not full-check).
# `flow status --no-auto-start` succeeds only if a server is already running.
assert_errors "$FLOW" status --no-auto-start --strip-root

# Stop the server before testing full-check.
"$FLOW" stop .

# `flow full-check` should always do a standalone foreground check,
# ignoring check_is_status=true.
assert_errors "$FLOW" full-check . --strip-root --show-all-errors

# Verify that `flow full-check` did NOT start a server.
# Exit code 6 = No_server_running.
assert_exit 6 "$FLOW" status --no-auto-start --strip-root 2>/dev/null
