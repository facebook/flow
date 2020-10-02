#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
assert_errors "$FLOW" status .
printf "\\nDelete dependency that was shadowing another dependency\\n"
mv dir/node_modules/B.js tmp/
assert_ok "$FLOW" force-recheck dir/node_modules/B.js
printf "Expect shadowed dependency, now unshadowed, to trigger error\\n"
assert_errors "$FLOW" status .
printf "\\nUndo delete to resurrect shadowing dependency\\n"
mv tmp/B.js dir/node_modules/
assert_ok "$FLOW" force-recheck dir/node_modules/B.js
printf "Expect shadowing dependency to trigger error\\n"
assert_errors "$FLOW" status .
rmdir tmp
