#! /bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

start_flow .

assert_ok "$FLOW" save-state --out .flow.saved_state > /dev/null

mv a.js_ a.js
echo "a.js" > .flow.saved_state_file_changes

assert_ok "$FLOW" stop

assert_ok "$FLOW" start --saved-state-fetcher local --saved-state-no-fallback --lazy-mode fs

assert_ok "$FLOW" force-recheck --focus b.js

echo "Parse error should be reported:"
echo ""
assert_errors "$FLOW" # parse error is reported

echo "(a: empty);" >> b.js
echo ""
echo "Parse error, but no typing error in b.js:"
echo ""
assert_errors "$FLOW" # this should be a parse error, but not a typing error (because of the parse error in a.js)
