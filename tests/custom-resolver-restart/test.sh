#!/bin/bash
# Set the resolver to use an extension that doesn't exist - resolutions should fail
echo 'ext = "wrong";' >> resolver.js
assert_ok "$FLOW" force-recheck resolver.js
assert_errors "$FLOW" status .

# Set the resolver back to the correct extensions - resolutions should work again
echo 'ext = "foo";' >> resolver.js
assert_ok "$FLOW" force-recheck resolver.js
assert_ok "$FLOW" status .
