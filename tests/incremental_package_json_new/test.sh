#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nAdd trivial package.json:\n"
echo {} > package.json

printf "\nAdd trivial error to file.js:\n"
echo "0 as 1;" > file.js

assert_ok "$FLOW" force-recheck --focus --no-auto-start package.json file.js

# sleep added to ensure server has died
sleep 2

# The following should succeed, since server should have died with the recheck
assert_ok "$FLOW" start --file-watcher none --lazy-mode none --wait .

assert_errors "$FLOW" status --no-auto-start . # errors on file.js
