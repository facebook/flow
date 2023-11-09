#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "========================================================================================================================"
echo "We start with an interface conformance error in Android file for logical module A"
echo "========================================================================================================================"
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Fix the interface conformance error in Android file for logical module A"
echo "========================================================================================================================"
echo "declare export default function A(): number;" > A.android.js
assert_ok "$FLOW" force-recheck A.android.js
echo "All interface conformance errors are fixed."
assert_ok "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Introduce an interface conformance error in iOS file for logical module A"
echo "========================================================================================================================"
echo "declare export default function A(): string;" > A.ios.js
assert_ok "$FLOW" force-recheck A.ios.js
echo "Now we have an interface conformance error in iOS file."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Change interface file to get a different set of conformance errors for logical module A"
echo "========================================================================================================================"
echo "declare export default function A(): string;" > A.js.flow
assert_ok "$FLOW" force-recheck A.js.flow
echo "Now we have an interface conformance error in Android file, but not in iOS file."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Deleting interface file, all conformance error disappears, but A cannot be imported without platform extension"
echo "========================================================================================================================"
mv A.js.flow A.js.ignored
assert_ok "$FLOW" force-recheck A.js.flow
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Add back interface file for logical module A"
echo "========================================================================================================================"
mv A.js.ignored A.js.flow
assert_ok "$FLOW" force-recheck A.js.flow
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Fix all type errors for logical module A"
echo "========================================================================================================================"
echo "declare export default function A(): number;" > A.js.flow
echo "declare export default function A(): number;" > A.ios.js
echo "declare export default function A(): number;" > A.android.js
assert_ok "$FLOW" force-recheck A.js.flow A.ios.js A.android.js
assert_ok "$FLOW" status .

echo "========================================================================================================================"
echo "Delete android file for logical module A"
echo "========================================================================================================================"
mv A.android.js A.android.ignore
assert_ok "$FLOW" force-recheck A.android.js
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Add back android file for logical module A"
echo "========================================================================================================================"
mv A.android.ignore A.android.js
assert_ok "$FLOW" force-recheck A.android.js
assert_ok "$FLOW" status .


echo "========================================================================================================================"
echo "Delete android file for logical module B"
echo "========================================================================================================================"
mv B.android.js B.android.ignore
assert_ok "$FLOW" force-recheck B.android.js
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""


echo "========================================================================================================================"
echo "Make logical module B iOS only"
echo "========================================================================================================================"
echo "// @supportsPlatform ios
declare export const b: number;" > B.js.flow
assert_ok "$FLOW" force-recheck B.js.flow
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""
