#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# The server starts in lazy mode. First trigger a slow recheck.
# This will check slow_dependent and dependency.
printf "\nTrigger a slow recheck: "
assert_ok "$FLOW" force-recheck --focus slow_dependent.js
printf "DONE\n"

# Sleep for a bit to give the recheck a chance to actually start
# rechecking the file (vs calculating deps, etc).
printf "\nSleep: "
sleep 1
printf "DONE\n"

start_time=$(date +"%s.%N")

# While that's running, try to get-def on fast_dependent, which
# also needs dependency. It should cancel the slow recheck and
# answer quickly.
printf "\nGet-def with unchecked dependency:\n"
assert_ok "$FLOW" get-def fast_dependent.js 5 2 --strip-root --json

printf "\nDid it finish in less than 2 seconds? "
end_time=$(date +"%s.%N")
echo "$start_time $end_time" | awk '{ s=$1; e=$2; print ((e - s) < 2) ? "YES" : "NO"; }'

# Then the slow recheck should restart and eventually finish.
printf "\nOriginal recheck finished:\n"
assert_errors "$FLOW" status --no-auto-start
