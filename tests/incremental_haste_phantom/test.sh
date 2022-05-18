#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nInitial status:\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nIntroduce haste module \"foo\"\n"
cp tmp/foo.js foo.js
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRemove haste module \"foo\"\n"
rm foo.js
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nDone!\n"
