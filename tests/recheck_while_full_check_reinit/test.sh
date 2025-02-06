#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "No errors initially:"
assert_ok "$FLOW" status .
echo ""

echo "Updating libdef and then removing a file should error:"
echo "declare const myGlobal: number" > lib/globals.js
assert_ok "$FLOW" force-recheck lib/globals.js
rm to_be_removed.js
assert_ok "$FLOW" force-recheck to_be_removed.js
assert_errors "$FLOW" status .
echo ""

echo "Adding back the removed file and then updating libdef should remove error:"
touch to_be_removed.js
assert_ok "$FLOW" force-recheck to_be_removed.js
echo "declare const myGlobal: string" > lib/globals.js
assert_ok "$FLOW" force-recheck lib/globals.js
assert_ok "$FLOW" status .

echo "Removing a file and then updating libdef should error:"
rm to_be_removed.js
assert_ok "$FLOW" force-recheck to_be_removed.js
echo "declare const myGlobal: number" > lib/globals.js
assert_ok "$FLOW" force-recheck lib/globals.js
assert_errors "$FLOW" status .
echo ""

echo "Updating libdef and add then addiing back the removed file should remove error:"
echo "declare const myGlobal: string" > lib/globals.js
assert_ok "$FLOW" force-recheck lib/globals.js
touch to_be_removed.js
assert_ok "$FLOW" force-recheck to_be_removed.js
assert_ok "$FLOW" status .
echo ""

echo "Remove declare module and add module type def together"
mkdir @flowtyped
echo "declare module.exports: number" > @flowtyped/foo.js.flow
rm lib/module_def.js
assert_ok "$FLOW" force-recheck lib/module_def.js @flowtyped/foo.js.flow
assert_errors "$FLOW" status .
echo ""
