#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" autofix insert-type --in-place a.js 5 18 5 21
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status --strip-root
