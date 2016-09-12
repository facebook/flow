#!/bin/sh

FLOW=$1

cp lib/lib.js lib/lib.js.orig

# This should not cause the Flow server to die
$FLOW force-recheck --no-auto-start lib/lib/js
echo "first status, after recheck"
$FLOW status --no-auto-start 2>/dev/null

# This also should not cause the Flow server to die
touch lib/lib.js
echo "second status, after touch"
$FLOW status --no-auto-start 2>/dev/null

# This should cause the flow server to die
cp lib/lib.js.modified lib/lib.js
echo "third status, after modification"
# This should have no output, since it won't find a server. It will print stuff
# on stderr but it includes the nondeterministically-chosen tmpdir so we can't
# compare against it.
$FLOW status --no-auto-start 2>/dev/null

echo "done"

mv lib/lib.js.orig lib/lib.js
