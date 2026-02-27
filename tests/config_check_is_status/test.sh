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
# Unset FLOW_TEMP_DIR so status looks in the default temp dir (same as check_is_status uses).
FLOW_TEMP_DIR= assert_errors "$FLOW" status --no-auto-start --strip-root
