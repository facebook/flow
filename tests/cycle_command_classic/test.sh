#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

printf "\\nStart server in classic mode\\n"
assert_ok "$FLOW" start .

printf "\\nValue cycle should include both files\\n"
assert_ok "$FLOW" cycle --strip-root fileA.js

printf "\\nType cycle should include both files\\n"
assert_ok "$FLOW" cycle --strip-root --types fileA.js
printf "\\n"
