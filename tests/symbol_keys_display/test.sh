#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "type-at-pos" "display_named.js"
queries_in_file "type-at-pos" "display_union.js"
queries_in_file "type-at-pos" "display_class.js"

echo "> insert-type insert_type.js 6 7"
assert_ok "$FLOW" autofix insert-type --in-place insert_type.js 6 7
echo "> insert-type insert_type.js 10 7"
assert_ok "$FLOW" autofix insert-type --in-place insert_type.js 10 7
assert_ok "$FLOW" force-recheck insert_type.js
echo "> cat insert_type.js"
cat insert_type.js
# The annotation insert-type just wrote must still check.
echo "> status"
assert_ok "$FLOW" status --strip-root
