#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

log_has() {
  awk -v needle="$1" 'index($0, needle) { found = 1 } END { exit !found }' "$FLOW_LOG_FILE"
}

printf "======The declaration path is initially absent======\n"
assert_errors "$FLOW" status .

printf "\n\n======Absent to global rebuilds builtins in place======\n"
cp global-string.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .
printf "Recoverable update requested full reinit: "
log_has "Will require full check reinit" && echo "yes" || echo "no"
printf "Scratch global-lib reinit ran: "
log_has "Global-lib recovery rebuilt" && echo "yes" || echo "no"

printf "\n\n======Global content replacement rebuilds builtins in place======\n"
cp global-number.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_ok "$FLOW" status .

printf "\n\n======Global to invalid removes the builtin======\n"
cp invalid.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .

printf "\n\n======Invalid to global rebuilds the builtin======\n"
cp global-string.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .

printf "\n\n======Global to external removes the builtin======\n"
cp external.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .

printf "\n\n======Live and clean global states agree======\n"
cp global-string.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .
assert_ok "$FLOW" stop .
start_flow .
assert_errors "$FLOW" status .

printf "\n\n======Global deletion removes the builtin======\n"
rm live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .

printf "\n\n======A batch can add multiple globals and an ordinary provider======\n"
cp batch-a.d.ts.ignored batch-a.d.ts
cp batch-b-number.d.ts.ignored batch-b.d.ts
cp batch-use.js.ignored batch-use.js
cp ordinary-string.js.ignored ordinary.js
assert_ok "$FLOW" force-recheck batch-a.d.ts batch-b.d.ts batch-use.js ordinary.js
assert_errors "$FLOW" status .
