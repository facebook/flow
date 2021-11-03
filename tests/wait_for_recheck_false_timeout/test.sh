#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# The server is running with --wait-for-recheck false. That means all commands
# will be parallelizable unless run with --wait-for-recheck true. This test
# asserts that these commands run without actually are waiting. Each should
# timeout.

printf "\\n\\nTrigger a 1000s recheck\\n"
mv sleep.js.ignored dependency.js
assert_ok "$FLOW" force-recheck dependency.js

printf "\\n\\nautocomplete with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" autocomplete \
  --timeout 1 --wait-for-recheck true \
  --strip-root --pretty focused.js 5 20 < focused.js

printf "\\n\\ncheck-contents with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" check-contents \
  --timeout 1 --wait-for-recheck true --strip-root focused.js < focused.js

printf "\\n\\ncoverage with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" coverage --timeout 1 --wait-for-recheck true focused.js

printf "\\n\\ndump-types with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" \
  dump-types --timeout 1 --wait-for-recheck true --strip-root focused.js

printf "\\n\\nfind-module with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" find-module \
  --timeout 1 --wait-for-recheck true --strip-root "./dependency" focused.js

printf "\\n\\nget-def with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" get-def \
  --timeout 1 --wait-for-recheck true --strip-root --pretty focused.js 5 18

printf "\\n\\nget-imports with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" get-imports \
  --timeout 1 --wait-for-recheck true --strip-root --pretty focused.js

printf "\\n\\ntype-at-pos with --wait-for-recheck true times out:\\n"
assert_exit 3 "$FLOW" type-at-pos \
  --timeout 1 --wait-for-recheck true --strip-root --pretty focused.js 5 18
