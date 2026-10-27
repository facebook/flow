#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "===== status: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" status --no-auto-start

printf "\\n===== full-check: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" full-check .

printf "\\n===== check-contents: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" check-contents < a.js

printf "\\n===== check-contents: other errors get no hint =====\\n"
printf "const y: string = 1;\\n" | assert_errors "$FLOW" check-contents

for i in 1 2 3 4 5; do
  printf "// %s\\n" "$i" > b.js
  assert_ok "$FLOW" force-recheck b.js
done
printf "\\n===== status: errors persisted through 5 rechecks, no inspect-types hint yet =====\\n"
assert_errors "$FLOW" status --no-auto-start

printf "// 6\\n" > b.js
assert_ok "$FLOW" force-recheck b.js
printf "\\n===== status: errors persisted through 6 rechecks point at inspect-types =====\\n"
assert_errors "$FLOW" status --no-auto-start

printf "\\n===== check-contents: no inspect-types hint =====\\n"
assert_errors "$FLOW" check-contents < a.js

printf "declare var x: boolean;\\n" > a.js
assert_ok "$FLOW" force-recheck a.js
printf "\\n===== status: errors fixed =====\\n"
assert_ok "$FLOW" status --no-auto-start
