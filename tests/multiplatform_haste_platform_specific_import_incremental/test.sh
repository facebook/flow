#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "In this test setup, we start with an iOS specific file and a common interface file with more general signature."
echo "An iOS file tries to import the logical module and expect the more specific type, which should pass."
echo "A general file tries to import the logical module and expect the more specific type, which should fail."
echo ""
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Remove A.ios.js"
mv A.ios.js A.ios.js.ignore
assert_ok "$FLOW" force-recheck A.ios.js
echo "Both iOSOnly and General should error."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Restore A.ios.js and remove A.js.flow"
mv A.ios.js.ignore A.ios.js
mv A.js.flow A.js.ignore
assert_ok "$FLOW" force-recheck A.ios.js A.js.flow
echo "General should error with unbound module."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Remove both A.ios.js and remove A.js.flow"
mv A.ios.js A.ios.js.ignore
assert_ok "$FLOW" force-recheck A.ios.js
echo "Both iOSOnly and General should error with unbound module."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Add back both A.ios.js and remove A.js.flow"
mv A.ios.js.ignore A.ios.js
mv A.js.ignore A.js.flow
assert_ok "$FLOW" force-recheck A.ios.js A.js.flow
echo "Only General should error."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""
