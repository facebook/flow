#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# With check_is_status=false, `flow check` should do a standalone full-check
# (not connect to a server), overriding the default check_is_status=true.
assert_errors "$FLOW" check . --strip-root --show-all-errors

# Verify that `flow check` did NOT start a server.
# Exit code 6 = No_server_running.
assert_exit 6 "$FLOW" status --no-auto-start --strip-root 2>/dev/null

# `flow full-check` should also do a standalone foreground check.
assert_errors "$FLOW" full-check . --strip-root --show-all-errors

# Verify that `flow full-check` did NOT start a server either.
assert_exit 6 "$FLOW" status --no-auto-start --strip-root 2>/dev/null
