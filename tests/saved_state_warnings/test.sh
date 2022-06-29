#! /bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Should not find any warnings because of lazy mode:"
assert_ok "$FLOW" status

echo
echo "Should not find any warnings even when including warnings:"
assert_ok "$FLOW" status --max-warnings 0

echo
echo "Focusing test.sh should find the warnings"
assert_ok "$FLOW" force-recheck --focus test.js
assert_errors "$FLOW" status --max-warnings 0
