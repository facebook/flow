#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# lazy_ref.js
printf "lazy_ref.js:14:9 = "
assert_ok "$FLOW" type-at-pos lazy_ref.js 14 9 --strip-root
printf "lazy_ref.js:19:9 = "
assert_ok "$FLOW" type-at-pos lazy_ref.js 19 9 --strip-root

# react_component.js
printf "react_component.js:3:9 = "
assert_ok "$FLOW" type-at-pos react_component.js 3 9 --strip-root
printf "react_component.js:13:33 = "
assert_ok "$FLOW" type-at-pos react_component.js 13 33 --strip-root
printf "react_component.js:18:17 = "
assert_ok "$FLOW" type-at-pos react_component.js 18 17 --strip-root
printf "react_component.js:31:7 = "
assert_ok "$FLOW" type-at-pos react_component.js 31 7 --strip-root --pretty --expand-json-output
printf "react_component.js:32:13 = "
assert_ok "$FLOW" type-at-pos react_component.js 32 13 --strip-root --pretty --expand-json-output
printf "react_component.js:32:29 = "
assert_ok "$FLOW" type-at-pos react_component.js 32 29 --strip-root --pretty --expand-json-output

queries_in_file "type-at-pos" "react.js"

printf "react_abstract_component.js:3:15 = "
assert_ok "$FLOW" type-at-pos react_abstract_component.js 3 15 --strip-root
