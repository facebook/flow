#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# The server is running with --wait-for-recheck false. That means all commands
# will be parallelizable unless run with --wait-for-recheck true. This test
# asserts that these commands run immediately without being blocked by the
# super long recheck

printf "\\n\\nTrigger a 1000s recheck\\n"
mv sleep.js.ignored dependency.js
assert_ok "$FLOW" force-recheck dependency.js

printf "\\n\\nautocomplete without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" autocomplete \
  --strip-root --pretty focused.js 5 20 < focused.js

printf "\\n\\ncheck-contents without --wait-for-recheck runs immediately:\\n"
assert_errors "$FLOW" check-contents --strip-root focused.js < focused.js

printf "\\n\\ncoverage without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" coverage focused.js

printf "\\n\\ndump-types without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" dump-types --strip-root focused.js

printf "\\n\\nfind-module without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" find-module --strip-root "./dependency" focused.js

printf "\\n\\nget-def without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" get-def --strip-root --pretty focused.js 5 18

printf "\\n\\nget-imports without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" get-imports \
  --strip-root --pretty focused.js

printf "\\n\\nsuggest without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" suggest --strip-root focused.js

printf "\\n\\ntype-at-pos without --wait-for-recheck runs immediately:\\n"
assert_ok "$FLOW" type-at-pos --strip-root --pretty focused.js 5 18
