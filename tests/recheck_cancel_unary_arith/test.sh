#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Regression test: a recheck that is canceled while a worker is inside
# `operators::unary_arith` must not panic. Cancellation deep in the checker
# surfaces as `FlowJsException::WorkerCanceled`, which the leaf operator
# helpers used to `.unwrap()`/`.expect("should not fail outside speculation")`
# instead of propagating to the `JobError::Canceled` boundary in
# `merge_service::mk_check`. The result was a worker-thread panic on every
# mid-check cancellation.

assert_ok "$FLOW" stop

# Wait for init.
start_flow .

# Generate a file whose check spends a long time inside `unary_arith`: tens of
# thousands of unary-minus operations, each over a union that must be
# concretized. Every concretization step polls `check_should_cancel`, so once
# the cancel flag flips this worker is overwhelmingly likely to observe it from
# inside `unary_arith`.
{
  printf '// @flow\n'
  printf 'declare var x: %s;\n' "$(seq -s '|' 1 80)"
  yes '(-x);' | head -n 40000
} > arith.js

# Kick off a recheck that hangs for 100s inside $Flow$DebugSleep (holding the
# "Checking files" phase open) while a sibling worker churns through arith.js.
cp sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js arith.js

# Give the sibling worker time to get deep into arith.js's unary_arith checks.
sleep 3

# Editing a dependent of the sleeping file cancels the in-flight recheck. On the
# buggy build, the arith.js worker observes WorkerCanceled inside unary_arith and
# panics; on a correct build the cancellation propagates and the recheck unwinds
# and rolls back cleanly.
echo " " >> sleep_dependent.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep_dependent.js

# Remove the sleeping file and let the recheck settle.
rm sleep.js sleep_dependent.js arith.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js sleep_dependent.js arith.js

# The server must survive and report the one real error (foo.js).
assert_errors "$FLOW" status --no-auto-start --timeout 30

# Regression guard: the canceled recheck must not have panicked a worker.
printf "\\nWorker panicked on canceled recheck? "
grep -q "should not fail outside speculation\\|called \`Result::unwrap()\` on an \`Err\` value: WorkerCanceled" "$FLOW_LOG_FILE" \
  && echo "Yes (BUG)" \
  || echo "No"
