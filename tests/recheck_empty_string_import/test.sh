#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"$FLOW" stop
echo "// @flow" > test.js
"$FLOW" start --wait
echo "// @flow
import '';" > test.js
"$FLOW" force-recheck --focus test.js --no-auto-start
assert_errors "$FLOW" status --no-auto-start
