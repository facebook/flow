#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

: > .flow.saved_state_file_changes

"$FLOW" start --saved-state-fetcher none --wait
"$FLOW" save-state --out .flow.saved_state >> /dev/null
"$FLOW" stop
"$FLOW" start --saved-state-fetcher local --saved-state-no-fallback --wait

printf "find-module resolves package main from saved state:\n"
assert_ok "$FLOW" find-module --strip-root ./pkg test.js
echo

printf "check-contents resolves package main from saved state:\n"
assert_ok "$FLOW" check-contents --strip-root test.js <<'EOF'
// @flow

import pkg from './pkg';
pkg as number;
EOF
echo

assert_ok "$FLOW" stop
