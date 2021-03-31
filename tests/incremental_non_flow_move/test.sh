#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nDelete non-@flow file foo.js:\n"
mv foo.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore non-@flow file foo.js:\n"
mv tmp/foo.js .
# NOTE: force-rechecking foo.js defeats the purpose of this particular test
# "$FLOW" force-recheck --no-auto-start foo.js
sleep 1
assert_ok "$FLOW" status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
