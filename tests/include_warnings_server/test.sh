#!/bin/sh
. ../assert.sh
FLOW=$1



printf "EMPTY FLOWCONFIG\n"
assert_ok "$FLOW" stop
cp empty_flowconfig .flowconfig
assert_ok "$FLOW" start

printf "\n\nCheck Without --include-warnings\n"
assert_errors "$FLOW" check --strip-root

printf "\n\nCheck With --include-warnings\n"
assert_errors "$FLOW" check --strip-root --include-warnings

printf "\n\nCheck-contents Without --include-warnings\n"
assert_errors "$FLOW" check-contents test.js --strip-root < test.js

printf "\n\nCheck-contents With --include-warnings\n"
assert_errors "$FLOW" check-contents test.js --strip-root --include-warnings < test.js

printf "\n\nStatus Without --include-warnings\n"
assert_errors "$FLOW" status --strip-root

printf "\n\nStatus With --include-warnings\n"
assert_errors "$FLOW" status --strip-root --include-warnings



printf "\n\n\n\nINCLUDE_WARNINGS FLOWCONFIG\n"
assert_ok "$FLOW" stop
cp include_warnings_flowconfig .flowconfig
assert_ok "$FLOW" start

printf "\n\nCheck Without --include-warnings\n"
assert_errors "$FLOW" check --strip-root

printf "\n\nCheck With --include-warnings\n"
assert_errors "$FLOW" check --strip-root --include-warnings

printf "\n\nCheck-contents Without --include-warnings\n"
assert_errors "$FLOW" check-contents test.js --strip-root < test.js

printf "\n\nCheck-contents With --include-warnings\n"
assert_errors "$FLOW" check-contents test.js --strip-root --include-warnings < test.js

printf "\n\nStatus Without --include-warnings\n"
assert_errors "$FLOW" status --strip-root

printf "\n\nStatus With --include-warnings\n"
assert_errors "$FLOW" status --strip-root --include-warnings
