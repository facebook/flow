#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "After start:\n"
assert_ok "$FLOW" status .

cp test1.js.fixture test.js
printf "\nAfter introducing a lint error:\n"
assert_ok "$FLOW" force-recheck test.js
assert_errors "$FLOW" status .

cp test2.js.fixture test.js
printf "\nAfter suppressing the lint error:\n"
assert_ok "$FLOW" force-recheck test.js
assert_ok "$FLOW" status .

cp test3.js.fixture test.js
printf "\nAfter fixing the lint error, leaving the suppression:\n"
assert_ok "$FLOW" force-recheck test.js
assert_ok "$FLOW" status . # 0 errors & 1 warning

cp test4.js.fixture test.js
printf "\nAfter removing the unused suppression:\n"
assert_ok "$FLOW" force-recheck test.js
assert_ok "$FLOW" status .
