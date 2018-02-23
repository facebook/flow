#!/bin/bash
. ../assert.sh
FLOW=$1

# Be careful: because we don't start a new Flow server for each request, the
# impact of one request here on the server's lazy mode state can impact the
# results of other requests. Reorder with care.

echo "finding global references for a class"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root foo.js 3 14
