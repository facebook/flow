#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that the "checking N files" count in lazy mode accurately reflects
# the number of files that were type-checked.
#
# a.js imports b.js which imports c.js. c.js has an internal type error.
# When only a.js is focused, c.js is a transitive dependency — it gets
# merged (for type signatures) but not type-checked. The bug: the old
# checked count included c.js as "checked" even though its error was not
# found, making the count misleading.

printf "==== Focus a.js ====\n"
assert_ok "$FLOW" force-recheck --focus a.js
assert_ok "$FLOW" status --show-lazy-status --no-auto-start --strip-root

status_output=$(assert_ok "$FLOW" status --no-auto-start --strip-root)
if [[ "$status_output" == *"The Flow server is currently in lazy mode"* ]]; then
  printf "Lazy status was shown without --show-lazy-status\n" >&2
  exit 1
fi

status_output=$(assert_ok "$FLOW" --no-auto-start --strip-root)
if [[ "$status_output" == *"The Flow server is currently in lazy mode"* ]]; then
  printf "Lazy status was shown by bare flow without --show-lazy-status\n" >&2
  exit 1
fi

status_output=$(assert_ok "$FLOW" --no-auto-start --strip-root --show-lazy-status)
if [[ "$status_output" != *"The Flow server is currently in lazy mode and is only checking 2/4 files."* ]]; then
  printf "Bare flow did not show lazy status with --show-lazy-status\n" >&2
  exit 1
fi
