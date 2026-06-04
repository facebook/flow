#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

"$FLOW" start --saved-state-fetcher none --wait

printf "status:\n"
assert_errors "$FLOW" status --no-auto-start --strip-root
echo

printf "find-module resolves excluded node package in haste mode:\n"
assert_ok "$FLOW" find-module --strip-root qux/docblock foo/bar/client.js
echo

printf "check-contents resolves excluded node package in haste mode:\n"
contents="$(< foo/bar/client.js)"
assert_errors "$FLOW" check-contents --no-auto-start --strip-root foo/bar/client.js <<< "$contents"

assert_ok "$FLOW" stop
