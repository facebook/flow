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

# Server restarts on incompatible package.json change. Wait, then stop the
# auto-restarted (lazy) server and start a non-lazy one to check all files.
sleep 2
"$FLOW" stop . 2>/dev/null || true
sleep 1
assert_ok "$FLOW" start --file-watcher none --lazy-mode none --wait .

assert_errors "$FLOW" status --no-auto-start . # errors on file.js
