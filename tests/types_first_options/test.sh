#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Show well_formed_export errors with .flowconfig:"
echo "types_first=true"
echo ""
assert_errors "$FLOW" status --strip-root

echo "Stopping server..."
assert_ok "$FLOW" stop

echo ""
echo "In .flowconfig:"
echo "types_first=true"
echo "well_formed_exports=false"
echo ""
mv .flowconfig.err1 .flowconfig
assert_exit "$EXIT_INVALID_FLOWCONFIG" "$FLOW" start . 2>&1

echo ""
echo "In .flowconfig:"
echo "well_formed_exports=false"
echo "types_first=true"
echo ""
mv .flowconfig.err2 .flowconfig
assert_exit "$EXIT_INVALID_FLOWCONFIG" "$FLOW" start . 2>&1

echo ""
echo "In .flowconfig:"
echo "well_formed_exports=true"
echo "well_formed_exports.includes=foo"
echo "types_first=true"
mv .flowconfig.err3 .flowconfig
assert_exit "$EXIT_INVALID_FLOWCONFIG" "$FLOW" start --types-first . 2>&1
