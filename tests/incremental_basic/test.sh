#!/bin/bash
. ../assert.sh
FLOW=$1

mkdir tmp
cp ./*.js tmp/
assert_errors "$FLOW" status .
cp tmp1/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
cp tmp2/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
cp tmp3/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
mv tmp/*.js ./
rmdir tmp
