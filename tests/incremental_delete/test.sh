#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp

printf "\nInitial status:\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete c.js:\n"
mv c.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start c.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete b.js:\n"
mv b.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start b.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore b.js:\n"
mv tmp/b.js .
assert_ok "$FLOW" force-recheck --no-auto-start b.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore c.js:\n"
mv tmp/c.js .
assert_ok "$FLOW" force-recheck --no-auto-start c.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete d.json:\n"
mv d.json tmp
assert_ok "$FLOW" force-recheck --no-auto-start d.json
assert_errors "$FLOW" status --no-auto-start .

mv tmp/d.json .

printf "\nDelete unchecked.js:\n"
mv unchecked.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start unchecked.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore unchecked.js:\n"
mv tmp/unchecked.js .
assert_ok "$FLOW" force-recheck --no-auto-start unchecked.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete dupe2.js:\n"
mv dupe2.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start dupe2.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore dupe2.js:\n"
mv tmp/dupe2.js .
assert_ok "$FLOW" force-recheck --no-auto-start dupe2.js
assert_errors "$FLOW" status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
