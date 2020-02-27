#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# declare.js
printf "declare.js:4:3 = "
assert_ok "$FLOW" type-at-pos declare.js 3 18 --strip-root --pretty

# function.js
printf "function.js:4:3 = "
assert_ok "$FLOW" type-at-pos function.js 4 3 --strip-root --pretty
printf "function.js:8:3 = "
assert_ok "$FLOW" type-at-pos function.js 8 3 --strip-root --pretty
printf "function.js:12:3 = "
assert_ok "$FLOW" type-at-pos function.js 12 3 --strip-root --pretty
printf "function.js:16:3 = "
assert_ok "$FLOW" type-at-pos function.js 16 3 --strip-root --pretty

# default.js
printf "default.js:4:16 = "
assert_ok "$FLOW" type-at-pos default.js 4 16 --strip-root --pretty
printf "default.js:5:17 = "
assert_ok "$FLOW" type-at-pos default.js 5 17 --strip-root --pretty

# function-poly-0.js
printf "function-poly-0.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 3 10 --strip-root --pretty
printf "function-poly-0.js:3:30 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 3 30 --strip-root --pretty
printf "function-poly-0.js:4:7 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 4 7 --strip-root --pretty

# function-poly-1.js
printf "function-poly-1.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 3 10 --strip-root --pretty
printf "function-poly-1.js:3:3 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 3 33 --strip-root --pretty
printf "function-poly-1.js:4:7 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 4 7 --strip-root --pretty

# function-poly-2.js
printf "function-poly-2.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 3 10 --strip-root --pretty
printf "function-poly-2.js:4:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 4 12 --strip-root --pretty
printf "function-poly-2.js:5:5 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 5 5 --strip-root --pretty
printf "function-poly-2.js:6:5 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 6 5 --strip-root --pretty
printf "function-poly-2.js:7:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 7 12 --strip-root --pretty
printf "function-poly-2.js:9:13 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 9 13 --strip-root --pretty
printf "function-poly-2.js:11:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 11 12 --strip-root --pretty

# function-poly-3.js
printf "function-poly-3.js:8:1 = "
assert_ok "$FLOW" type-at-pos function-poly-3.js 8 1 --strip-root --pretty

# function-poly-4.js
printf "function-poly-4.js:3:11 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 3 11 --strip-root --pretty
printf "function-poly-4.js:7:7 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 7 7 --strip-root --pretty
printf "function-poly-4.js:9:7 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 9 7 --strip-root --pretty

# function-poly-5.js
printf "function-poly-5.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-5.js 3 10 --strip-root --pretty
