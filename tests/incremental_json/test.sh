#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nInitial status:\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nCreate data.json:\n"
cp data.json1 data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_ok "$FLOW" status --no-auto-start .

printf "\nModify data.json:\n"
cp data.json2 data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete data.json:\n"
rm data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_errors "$FLOW" status --no-auto-start .

printf "\nDone!\n"
