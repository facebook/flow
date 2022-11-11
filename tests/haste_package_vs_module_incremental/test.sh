#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "'package' module resolves to file:\n"
assert_errors "$FLOW" status
echo

printf "duplicate provider:\n"
mv fixtures/package package
# changing a package will restart the server, so update saved_state_file_changes
if [ -f .flow.saved_state_file_changes ]; then
  printf "package/package.json\npackage/index.js\n" > .flow.saved_state_file_changes
fi
assert_ok "$FLOW" force-recheck package/package.json package/index.js
assert_errors "$FLOW" status
echo

printf "'package' module resolves to package:\n"
mv package.js fixtures/package.js
assert_ok "$FLOW" force-recheck package.js
assert_errors "$FLOW" status
echo

printf "duplicate provider:\n"
mv fixtures/package.js package.js
assert_ok "$FLOW" force-recheck package.js
assert_errors "$FLOW" status
echo

printf "'package' module resolves to file again:\n"
mv package fixtures/package
# changing a package will restart the server, so update saved_state_file_changes
if [ -f .flow.saved_state_file_changes ]; then
  echo "" > .flow.saved_state_file_changes
fi
assert_ok "$FLOW" force-recheck package/package.json package/index.js
assert_errors "$FLOW" status
