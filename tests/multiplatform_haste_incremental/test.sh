#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "We start with an interface conformance error in Android file"
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Fix the interface conformance error in Android file"
echo "declare export default function A(): number;" > A.android.js
assert_ok "$FLOW" force-recheck A.android.js
echo "All interface conformance errors are fixed."
assert_ok "$FLOW" status .
echo ""
echo ""
echo ""

echo "Introduce an interface conformance error in iOS file"
echo "declare export default function A(): string;" > A.ios.js
assert_ok "$FLOW" force-recheck A.ios.js
echo "Now we have an interface conformance error in iOS file."
assert_errors "$FLOW" status .
echo ""
echo ""
echo ""

echo "Change interface file to get a different set of conformance errors"
echo "declare export default function A(): string;" > A.js.flow
assert_ok "$FLOW" force-recheck A.js.flow
echo "Now we have an interface conformance error in Android file, but not in iOS file."
assert_errors "$FLOW" status .
