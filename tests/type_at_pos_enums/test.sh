#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "type-at-pos" "test.js"

# Enum member sites frame like member references, even though members carry
# no types of their own. Queried in friendly mode to observe the framing.
printf "test.js:3:9 (framed) = "
assert_ok "$FLOW" type-at-pos test.js 3 9 --strip-root
printf "test.js:3:12 (framed) = "
assert_ok "$FLOW" type-at-pos test.js 3 12 --strip-root
