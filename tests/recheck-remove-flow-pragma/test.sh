#!/bin/bash

printf "\\nServer should start in types-first mode\\n"
start_flow .
assert_errors "$FLOW" status --strip-root

printf "\\nRemoving the @flow pragma from foo.js should fix the errors\\n"
sed -i -e 's/@flow/@noflow/' foo.js
assert_ok "$FLOW" force-recheck foo.js
assert_ok "$FLOW" status --strip-root

printf "\\nAdding the @flow pragma back to foo.js should make the errors return\\n"
sed -i -e 's/@noflow/@flow/' foo.js
assert_ok "$FLOW" force-recheck foo.js
assert_errors "$FLOW" status --strip-root

assert_ok "$FLOW" stop
