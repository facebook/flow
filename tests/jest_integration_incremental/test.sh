#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "========================================================================================================================"
echo "We start with an error because ./module doesn't exist"
echo "========================================================================================================================"
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Fix the error by introducing module.js"
echo "========================================================================================================================"
touch module.js
assert_ok "$FLOW" force-recheck module.js
assert_ok "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Reintroduce the error by removing module.js"
echo "========================================================================================================================"
rm module.js
assert_ok "$FLOW" force-recheck module.js
assert_errors "$FLOW" status .
