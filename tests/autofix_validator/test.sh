#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" autofix insert-type --in-place simplify-empty.js 3 28
echo "> cat simplify-empty.js"
cat simplify-empty.js
assert_ok "$FLOW" force-recheck simplify-empty.js
echo "> flow status"
assert_ok "$FLOW" status
