#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "type-at-pos" "component.js"
queries_in_file "type-at-pos" "render_types.js"
printf "render_types.js:8:7 = "
assert_ok "$FLOW" type-at-pos render_types.js 8 7 --expand-json-output --pretty --strip-root
