#!/bin/bash
# Be careful: because we don't start a new Flow server for each request, the
# impact of one request here on the server's lazy mode state can impact the
# results of other requests. Reorder with care.

echo "finding global references for a class member, from the use site"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root bar.js 5 13
