#!/bin/bash
. ../assert.sh
FLOW=$1

echo 'ext = "wrong";' >> resolver.js
assert_ok $FLOW force-recheck hello.foo.js world.foo.js
assert_errors $FLOW status .

echo 'ext = "foo";' >> resolver.js
assert_ok $FLOW force-recheck hello.foo.js world.foo.js
assert_ok $FLOW status .
