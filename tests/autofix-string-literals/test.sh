#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" autofix insert-type --in-place a.js 6 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 7 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 8 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 9 9
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status
