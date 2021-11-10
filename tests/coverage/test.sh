#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "coverage of declare module:\n"
assert_ok "$FLOW" coverage --color declare_module.js

printf "\nshould not crash:\n"
assert_ok "$FLOW" coverage --color crash.js

printf "\nshould terminate:\n"
assert_ok "$FLOW" coverage --color non-termination.js

printf "\nerrors because not @flow:\n"
assert_ok "$FLOW" coverage --quiet no_pragma.js 2>&1

printf "\nforces @flow:\n"
assert_ok "$FLOW" coverage --quiet --all no_pragma.js 2>&1

#
# some more detailed tests:
#

echo "-----------------------------"
echo "call.js"
echo "-----------------------------"
echo
assert_ok "$FLOW" coverage --strip-root --pretty call.js

echo "-----------------------------"
echo "coverage.js"
echo "-----------------------------"
echo
assert_ok "$FLOW" coverage --strip-root --pretty coverage.js

echo "-----------------------------"
echo "any-refinement.js"
echo "-----------------------------"
echo
assert_ok "$FLOW" coverage --strip-root --pretty any-refinement.js

echo "-----------------------------"
echo "unicode.js"
echo "-----------------------------"
echo
assert_ok "$FLOW" coverage --strip-root --pretty unicode.js
assert_ok "$FLOW" coverage --color unicode.js

"$FLOW" stop
"$FLOW" start --trust-mode=check
echo "-----------------------------"
echo "trust.js"
echo "-----------------------------"
echo
assert_ok "$FLOW" coverage --show-trust --strip-root --pretty trust.js
assert_ok "$FLOW" coverage --show-trust trust.js
