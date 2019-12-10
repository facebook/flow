#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

. ../fs.sh

mkdir tmp
cp ./*.js tmp/

printf "\nTest a:\n"
assert_errors "$FLOW" status .
copy tmp1a/a1.js a1.js
assert_errors "$FLOW" status .
copy tmp2a/a1.js a1.js
assert_errors "$FLOW" status .
remove a*.js

printf "\nTest b:\n"
copy tmp1b/b1.js b1.js
assert_errors "$FLOW" status .
copy tmp2b/b0.js b0.js
assert_ok "$FLOW" status .
remove b*.js

printf "\nTest c:\n"
copy tmp1c/c2.js ./c2.js
assert_errors "$FLOW" status .
copy tmp2c/c1.js ./c1.js
assert_errors "$FLOW" status .
remove c*.js

printf "\nTest d:\n"
copy tmp1d/d1.js ./d1.js
assert_errors "$FLOW" status .
remove d*.js

printf "\nTest e:\n"
copy tmp1e/e2.js ./e2.js
assert_errors "$FLOW" status .
copy tmp2e/e1.js ./e1.js
assert_ok "$FLOW" status .
copy tmp3e/e1.js e1.js
copy tmp3e/e2.js e2.js
assert_ok "$FLOW" status .
remove e*.js

printf "\nTest f:\n"
copy tmp1f/f1.js ./f1.js
assert_errors "$FLOW" status .
copy tmp2f/f1.js ./f1.js
assert_ok "$FLOW" status .
copy tmp3f/f1.js ./f1.js
assert_ok "$FLOW" status .
copy tmp4f/f1.js ./f1.js
assert_errors "$FLOW" status .
remove f*.js

printf "\nTest g:\n"
copy tmp1g/g1.js ./g1.js
assert_ok "$FLOW" status .
remove g*.js

printf "\nTest h:\n"
copy tmp1h/h1.js ./h1.js
assert_errors "$FLOW" status .
remove h*.js

printf "\nTest i:\n"
copy tmp1i/i1.js ./i1.js
assert_errors "$FLOW" status .
remove i*.js

printf "\nTest j:\n"
copy tmp1j/j1.js ./j1.js
assert_errors "$FLOW" status .
remove j*.js

printf "\nTest l:\n"
copy tmp1l/l1.js ./l1.js
assert_ok "$FLOW" status .
remove l*.js

printf "\nTest m:\n"
copy tmp1m/m1.js ./m1.js
sleep 2;
assert_errors "$FLOW" status .
remove m*.js

# TODO: move this to another file, as it kills the server
printf "\nTest k:\n"
copy tmplibk/libk1.js lib/libk1.js
copy tmpk/k.js ./k.js
assert_errors "$FLOW" status .
copy tmplibk/libk2.js lib/libk2.js
assert_ok "$FLOW" status .
remove lib/libk2.js
assert_errors "$FLOW" status .
remove k.js lib/*.js

mv tmp/*.js ./
rmdir tmp
