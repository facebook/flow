#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

start_flow . --lazy

assert_ok "$FLOW" status --strip-root
assert_ok "$FLOW" force-recheck --focus c.js
assert_ok "$FLOW" status --strip-root

sed -i'.orig' -e "s/XXX//g" "a.js"
echo "export type X = empty;" >> b2.js
assert_ok "$FLOW" force-recheck --focus a.js
assert_ok "$FLOW" force-recheck --focus b2.js

assert_ok "$FLOW" status --strip-root
