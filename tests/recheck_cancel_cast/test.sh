#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Regression test: a recheck that is canceled while a worker is inside the
# type-cast checking path (`type_operation_utils::perform_type_cast`, reached
# from `as`/`satisfies`/legacy-colon casts in `expression_`) must not panic.
# Cancellation deep in the checker surfaces as `FlowJsException::WorkerCanceled`,
# which the cast sites used to `.unwrap()` instead of propagating to the
# `JobError::Canceled` boundary in `merge_service::mk_check`. The result was a
# worker-thread panic on every mid-check cancellation.

assert_ok "$FLOW" stop

# Wait for init.
start_flow .

# Generate a file whose check spends many seconds inside a SINGLE
# `perform_type_cast` call. Each `as` cast flows an 800-member object union
# against a disjoint 800-member object union, so one cast drives ~800*800
# `FlowJs::flow` subtype checks -- several seconds of work, all inside one
# `perform_type_cast`, polling `check_should_cancel` densely throughout.
#
# The cast must be this expensive *per operation*, not merely high-volume: the
# cancel flag is sticky, so a worker churning through many *cheap* casts almost
# always observes the flag at a statement boundary between casts (where the
# cancel propagates cleanly via `?`) rather than mid-cast. Only a cast that
# keeps the worker inside `perform_type_cast` for seconds reliably makes the
# poll that observes the cancel fire from *within* the cast. Several such casts
# back-to-back keep a worker mid-cast across the whole `sleep` window below,
# regardless of machine speed.
{
  printf '// @flow\n'
  printf 'declare var x: %s;\n' "$(seq 0 799 | sed 's/.*/{a&: &}/' | paste -sd '|' -)"
  printf 'type Dst = %s;\n' "$(seq 0 799 | sed 's/.*/{b&: &}/' | paste -sd '|' -)"
  yes '(x as Dst);' | head -n 8
} > cast.js

# Kick off a recheck that hangs for 100s inside $Flow$DebugSleep (holding the
# "Checking files" phase open) while a sibling worker churns through cast.js.
cp sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js cast.js

# Give the sibling worker time to get deep into cast.js's perform_type_cast checks.
sleep 3

# Editing a dependent of the sleeping file cancels the in-flight recheck. On the
# buggy build, the cast.js worker observes WorkerCanceled inside perform_type_cast
# and panics; on a correct build the cancellation propagates and the recheck
# unwinds and rolls back cleanly.
echo " " >> sleep_dependent.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep_dependent.js

# Remove the sleeping file and let the recheck settle.
rm sleep.js sleep_dependent.js cast.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js sleep_dependent.js cast.js

# The server must survive and report the one real error (foo.js).
assert_errors "$FLOW" status --no-auto-start --timeout 30

# Regression guard: the canceled recheck must not have panicked a worker.
printf "\\nWorker panicked on canceled recheck? "
grep -q "should not fail outside speculation\\|called \`Result::unwrap()\` on an \`Err\` value: WorkerCanceled" "$FLOW_LOG_FILE" \
  && echo "Yes (BUG)" \
  || echo "No"
