#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# coverage of declare module
assert_ok "$FLOW" coverage --color declare_module.js

# should not crash
assert_ok "$FLOW" coverage --color crash.js

# should terminate
assert_ok "$FLOW" coverage --color non-termination.js

# assumes @flow weak
assert_ok "$FLOW" coverage no_pragma.js

# assumes @flow weak
assert_ok "$FLOW" coverage --all no_pragma.js

# should be 0%
assert_ok "$FLOW" coverage --respect-pragma no_pragma.js

# --all wins (and assumes @flow weak)
assert_ok "$FLOW" coverage --respect-pragma --all no_pragma.js

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
