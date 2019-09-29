#!/bin/bash

mkdir tmp
cp foo.js tmp/

printf "\\nServer should start in types-first mode\\n"
start_flow .
assert_errors "$FLOW" status --strip-root

printf "\\nIntroducing a parse error should invalidate errors in dependents\\n"
cp tmp1/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_errors "$FLOW" status --strip-root

printf "\\nFixing a type error in a previously unparsed file should make the error go away\\n"
cp tmp2/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_ok "$FLOW" status --strip-root

printf "\\nRevert to initial state\\n"
cp tmp/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_errors "$FLOW" status --strip-root

printf "\\nAdd a recursive dependent\\n"
cp tmp3/qux.js qux.js
assert_ok "$FLOW" force-recheck --focus qux.js
assert_errors "$FLOW" status --strip-root

printf "\\nIntroducing a parse error should invalidate errors in dependents\\n"
cp tmp1/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_errors "$FLOW" status --strip-root

printf "\\nFixing a type error in a previously unparsed file should make the error go away\\n"
cp tmp2/foo.js foo.js
assert_ok "$FLOW" force-recheck --focus foo.js
assert_ok "$FLOW" status --strip-root

assert_ok "$FLOW" stop

cp tmp/foo.js foo.js
rm -rf tmp
