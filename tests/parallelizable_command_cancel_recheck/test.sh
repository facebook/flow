#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nNo errors thanks to lazy mode:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root

printf "\nAutocomplete should kick off a recheck:\n"
assert_ok "$FLOW" autocomplete --strip-root --wait-for-recheck false \
  focused.js 5 3 < focused.js.stdin

printf "\nWe still don't see the errors, since we don't have to check the dependency:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root

printf "\nNow we see the errors, since we've focused the dependency:\n"
assert_ok "$FLOW" force-recheck --focus dependency.js
assert_errors "$FLOW" status --no-auto-start
