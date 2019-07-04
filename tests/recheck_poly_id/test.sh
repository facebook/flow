#!/bin/bash

mkdir tmp
cp a.js tmp/

start_flow .

printf "\\nSubtyping over type applications of the same polymorphic type unifies their type arguments.\\n"
assert_errors "$FLOW" status --strip-root

printf "\\nAn upstream edit that does not touch the polymorphic type should preserve this behavior.\\n"
cp tmp1/a.js a.js
assert_ok "$FLOW" force-recheck --focus a.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop

mv tmp/a.js a.js
rmdir tmp
