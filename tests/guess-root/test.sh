#!/bin/bash

echo "nothing"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" type-at-pos 3 5 < d0.js

echo "with file"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" type-at-pos no-config/d1.js 3 5

echo "with file and config"
assert_ok "$FLOW" type-at-pos --strip-root config/no-config/d2.js 3 5

echo "with bad root directory"
assert_ok "$FLOW" type-at-pos --root ./config/no-config 3 5 < ./config/no-config/d2.js

echo "with bad root directory, and file"
assert_ok "$FLOW" type-at-pos --strip-root --root ./config/no-config ./config/no-config/d2.js 3 5

echo "with bad root directory, and file"
assert_ok "$FLOW" type-at-pos --strip-root --root ./config/no-config ./config/no-config/d2.js 3 5

echo "with file batch coverage"
assert_ok "$FLOW" batch-coverage --strip-root ./config/no-config/d2.js

echo "with bad root directory, and file batch coverage"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" batch-coverage --root ./config/no-config ./config/no-config/d2.js
