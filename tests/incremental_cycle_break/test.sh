#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nDelete A.js:\n"
mv A.js A.js.ignored
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore A.js:\n"
mv A.js.ignored A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
