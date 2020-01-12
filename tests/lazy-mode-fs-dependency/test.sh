#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nServer should start in fs lazy mode\\n"
start_flow . --lazy-mode fs

assert_ok "$FLOW" status --strip-root

printf "\\nFocus a file\\n"
assert_ok "$FLOW" force-recheck --focus foo.js
assert_errors "$FLOW" status --strip-root

printf "\\nEdit a dependency\\n"
cp tmp1/node_modules_bar_index.js node_modules/bar/index.js
assert_ok "$FLOW" force-recheck node_modules/bar/index.js
assert_ok "$FLOW" status --strip-root

printf "\\nRevert edit\\n"
cp tmp2/node_modules_bar_index.js node_modules/bar/index.js
assert_ok "$FLOW" force-recheck node_modules/bar/index.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop
