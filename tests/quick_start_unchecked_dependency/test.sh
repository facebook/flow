#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
start_flow . --lazy

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\n\"Warm up\" a dependency.\n"
echo "// change" >> b.js
assert_ok "$FLOW" force-recheck b.js
assert_ok "$FLOW" status --no-auto-start .

printf "\n\"Perturb\" a dependent that depends on the warmed up, but unchanged dependency.\n"
echo "// change" >> e.js
assert_ok "$FLOW" force-recheck e.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nTest that the perturbed dependent is actually known (expecting a crash otherwise).\n"
echo "// change" >> d.js
assert_ok "$FLOW" force-recheck d.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
