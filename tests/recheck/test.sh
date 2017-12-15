#!/bin/bash
. ../assert.sh
FLOW=$1
mkdir tmp
cp ./*.js tmp/

printf "\nTest a:\n"
assert_errors "$FLOW" status .
cp tmp1a/a1.js ./
assert_ok "$FLOW" force-recheck a1.js
assert_errors "$FLOW" status .
cp tmp2a/a1.js ./
assert_ok "$FLOW" force-recheck a1.js
assert_errors "$FLOW" status .
rm a*.js

printf "\nTest b:\n"
cp tmp1b/b1.js ./
assert_ok "$FLOW" force-recheck b1.js
assert_errors "$FLOW" status .
cp tmp2b/b0.js ./
assert_ok "$FLOW" force-recheck b0.js
assert_ok "$FLOW" status .
rm b*.js

printf "\nTest c:\n"
cp tmp1c/c2.js ./
assert_ok "$FLOW" force-recheck c2.js
assert_errors "$FLOW" status .
cp tmp2c/c1.js ./
assert_ok "$FLOW" force-recheck c1.js
assert_errors "$FLOW" status .
rm c*.js

printf "\nTest d:\n"
cp tmp1d/d1.js ./
assert_ok "$FLOW" force-recheck d1.js
assert_errors "$FLOW" status .
rm d*.js

printf "\nTest e:\n"
cp tmp1e/e2.js ./
assert_ok "$FLOW" force-recheck e2.js
assert_errors "$FLOW" status .
cp tmp2e/e1.js ./
assert_ok "$FLOW" force-recheck e1.js
assert_ok "$FLOW" status .
cp tmp3e/e1.js tmp3e/e2.js ./
assert_ok "$FLOW" force-recheck e1.js e2.js
assert_ok "$FLOW" status .
rm e*.js

printf "\nTest f:\n"
cp tmp1f/f1.js ./
assert_ok "$FLOW" force-recheck f1.js
assert_errors "$FLOW" status .
cp tmp2f/f1.js ./
assert_ok "$FLOW" force-recheck f1.js
assert_ok "$FLOW" status .
cp tmp3f/f1.js ./
assert_ok "$FLOW" force-recheck f1.js
assert_ok "$FLOW" status .
cp tmp4f/f1.js ./
assert_ok "$FLOW" force-recheck f1.js
assert_errors "$FLOW" status .
rm f*.js

printf "\nTest g:\n"
cp tmp1g/g1.js ./
assert_ok "$FLOW" force-recheck g1.js
assert_ok "$FLOW" status .
rm g*.js

printf "\nTest h:\n"
cp tmp1h/h1.js ./
assert_ok "$FLOW" force-recheck h1.js
assert_errors "$FLOW" status .
rm h*.js

printf "\nTest i:\n"
cp tmp1i/i1.js ./
assert_ok "$FLOW" force-recheck i1.js
assert_errors "$FLOW" status .
rm i*.js

printf "\nTest j:\n"
cp tmp1j/j1.js ./
assert_ok "$FLOW" force-recheck j1.js
assert_errors "$FLOW" status .
rm j*.js

printf "\nTest l:\n"
cp tmp1l/l1.js ./
assert_ok "$FLOW" force-recheck l1.js
assert_ok "$FLOW" status .
rm l*.js

printf "\nTest m:\n"
cp tmp1m/m1.js ./
assert_ok "$FLOW" force-recheck m1.js
sleep 2;
assert_errors "$FLOW" status .
rm m*.js

# TODO: move this to another file, as it kills the server
printf "\nTest k:\n"
cp tmplibk/libk1.js lib/
cp tmpk/k.js ./
assert_ok "$FLOW" force-recheck k.js lib/libk1.js
assert_errors "$FLOW" status .
cp tmplibk/libk2.js lib/
assert_ok "$FLOW" force-recheck lib/libk2.js
assert_ok "$FLOW" status .
rm lib/libk2.js
assert_ok "$FLOW" force-recheck lib/libk2.js
assert_errors "$FLOW" status .
rm k.js
rm lib/*.js

mv tmp/*.js ./
rmdir tmp
