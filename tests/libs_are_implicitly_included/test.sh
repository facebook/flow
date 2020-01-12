#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "Stopping existing server\n"
assert_ok "$FLOW" stop

printf "\nStarting flow\n"
start_flow src --no-auto-restart

printf "\nAssert flow is running\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\nChange lib file\n"
cp lib.js.modified lib.js
sleep 2 # We can't force recheck here since we're testing the file watching

printf "\nAssert flow is not running\n"
assert_exit 6 "$FLOW" status --no-auto-start src
