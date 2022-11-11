#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "'package/sub' does not resolve:\n"
assert_errors "$FLOW" status
echo

# package.js and package/package.json are duplicate providers. we pick package.js,
# which means `package/sub` doesn't resolve.
printf "duplicate provider:\n"
mv fixtures/package package
# changing a package will restart the server, so update saved_state_file_changes
if [ -f .flow.saved_state_file_changes ]; then
  printf "package/package.json\npackage/index.js\npackage/sub.js\n" > .flow.saved_state_file_changes
fi
assert_ok "$FLOW" force-recheck package/package.json package/index.js package/sub.js
assert_errors "$FLOW" status
echo

# changing `package` should force `test` to get rechecked -- this tests whether
# `test` has the proper phantom dependents to notice that package/package.json is now
# the provider.
printf "'package/sub' module resolves to package:\n"
mv package.js fixtures/package.js
assert_ok "$FLOW" force-recheck package/package.json package/index.js package/sub.js package.js
assert_errors "$FLOW" status
echo

printf "'package/sub' does not resolve again:\n"
mv package fixtures/package
mv fixtures/package.js package.js
# changing a package will restart the server, so update saved_state_file_changes
if [ -f .flow.saved_state_file_changes ]; then
  echo "" > .flow.saved_state_file_changes
fi
assert_ok "$FLOW" force-recheck package/package.json package/index.js package/sub.js package.js
assert_errors "$FLOW" status
