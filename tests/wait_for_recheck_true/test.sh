#!/bin/bash

# The server is running with --wait-for-recheck true. That means all commands
# will be nonparallelizable if run without --wait-for-recheck false. This test
# asserts that these commands run immediately without being blocked by the
# super long recheck

printf "\\n\\nTrigger a 1000s recheck\\n"
mv sleep.js.ignored dependency.js
assert_ok "$FLOW" force-recheck dependency.js

printf "\\n\\nautocomplete with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" autocomplete \
  --strip-root --pretty --wait-for-recheck false focused.js 5 20 < focused.js

printf "\\n\\ncheck-contents with --wait-for-recheck false runs immediately:\\n"
assert_errors "$FLOW" check-contents \
  --strip-root --wait-for-recheck false focused.js < focused.js

printf "\\n\\ncoverage with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" coverage --wait-for-recheck false focused.js

printf "\\n\\ndump-types with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" dump-types --strip-root --wait-for-recheck false focused.js

printf "\\n\\nfind-module with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" find-module \
  --strip-root --wait-for-recheck false "./dependency" focused.js

printf "\\n\\nget-def with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" get-def \
  --strip-root --pretty --wait-for-recheck false focused.js 5 18

printf "\\n\\nget-imports with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" get-imports \
  --strip-root --pretty --wait-for-recheck false focused.js

printf "\\n\\nsuggest with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" suggest --strip-root --wait-for-recheck false focused.js

printf "\\n\\ntype-at-pos with --wait-for-recheck false runs immediately:\\n"
assert_ok "$FLOW" type-at-pos \
  --strip-root --pretty --wait-for-recheck false focused.js 5 18
