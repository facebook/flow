#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "========================================================================================================================"
echo "We start with an interface conformance error in Android file"
echo "========================================================================================================================"
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Fix the interface conformance error in Android file"
echo "========================================================================================================================"
echo "declare export default function A(): number;" > A.android.js
assert_ok "$FLOW" force-recheck A.android.js
echo "All interface conformance errors are fixed."
assert_ok "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Introduce an interface conformance error in iOS file"
echo "========================================================================================================================"
echo "declare export default function A(): string;" > A.ios.js
assert_ok "$FLOW" force-recheck A.ios.js
echo "Now we have an interface conformance error in iOS file."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Change interface file to get a different set of conformance errors"
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
echo "Add back interface file"
echo "========================================================================================================================"
mv A.js.ignored A.js.flow
assert_ok "$FLOW" force-recheck A.js.flow
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Fix all type errors"
echo "========================================================================================================================"
echo "declare export default function A(): number;" > A.js.flow
echo "declare export default function A(): number;" > A.ios.js
echo "declare export default function A(): number;" > A.android.js
assert_ok "$FLOW" force-recheck A.js.flow A.ios.js A.android.js
assert_ok "$FLOW" status .

echo "========================================================================================================================"
echo "Delete android file"
echo "========================================================================================================================"
mv A.android.js A.android.ignore
assert_ok "$FLOW" force-recheck A.android.js
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "========================================================================================================================"
echo "Add back android file"
echo "========================================================================================================================"
mv A.android.ignore A.android.js
assert_ok "$FLOW" force-recheck A.android.js
assert_ok "$FLOW" status .
