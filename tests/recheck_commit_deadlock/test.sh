#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Regression test for the "collating errors" hang.
#
# `check-contents` type-checks on its connection thread, and the typed artifacts it
# produces are cached there along with the heap transaction that produced them. A
# transaction holds a read guard on the committed heap for as long as it is alive, and
# publishing a recheck needs the matching write guard. So if the cached transaction keeps
# its guard past the command, the next recheck can never publish: the commit waits on
# `state.write()` forever, the server sits at 0% CPU, and `flow status` reports
# "Server is rechecking (collating errors)" indefinitely.
#
# The guard must be handed back when the command finishes, even though the cache goes on
# holding the transaction itself.

printf "==== check-contents caches typed artifacts (and their transaction) ====\\n"
assert_ok "$FLOW" check-contents --strip-root file.js < file.js

printf "\\n==== a recheck must still be able to publish ====\\n"
printf "\\nexport const z: number = 2;\\n" >> dependency.js
assert_ok "$FLOW" force-recheck dependency.js

# Times out before the fix: the recheck's commit is blocked behind the guard that the
# check-contents transaction left behind in the connection thread's cache.
assert_ok "$FLOW" status --no-auto-start --timeout 30
