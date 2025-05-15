#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "We start with conflicting Haste module providers"
assert_errors "$FLOW" status .
echo ""

echo "Rename .js into .js.flow, still conflicting"
mv web/JsConflict.js web/JsConflict.js.flow
assert_ok "$FLOW" force-recheck web/JsConflict.js web/JsConflict.js.flow
assert_errors "$FLOW" status .
echo ""

echo "Fix conflicting Haste module providers errors"
mv web/JsConflict.js.flow web/JsNoConflict.js
assert_ok "$FLOW" force-recheck web/JsConflict.js.flow web/JsNoConflict.js
assert_ok "$FLOW" status .
echo ""

echo "Reintroduce errors"
mv web/JsNoConflict.js web/JsConflict.js
assert_ok "$FLOW" force-recheck web/JsNoConflict.js web/JsConflict.js
assert_errors "$FLOW" status .

echo "Introduce interface conformance errors"
echo "export type T = string" > web/JsFlowConflictTypeProvider.js
assert_ok "$FLOW" force-recheck web/JsFlowConflictTypeProvider.js
assert_errors "$FLOW" status .
echo ""

echo "Fix interface conformance errors"
echo "export type T = number" > web/JsFlowConflictTypeProvider.js
assert_ok "$FLOW" force-recheck web/JsFlowConflictTypeProvider.js
assert_errors "$FLOW" status .
echo ""
