#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Regression test: a recheck that is canceled while a worker is inside the
# optional-chain checking path (`optional_chain`'s `normalize_voided_out`,
# `handle_new_chain`'s `optional_chain::run` / `possible_concrete_types_for_inspection`)
# must not panic. Cancellation deep in the checker surfaces as
# `FlowJsException::WorkerCanceled`, which those sites used to
# `.unwrap()`/`.expect("should not fail outside speculation")` instead of
# propagating to the `JobError::Canceled` boundary in `merge_service::mk_check`.
# The result was a worker-thread panic on every mid-check cancellation.

assert_ok "$FLOW" stop

# Wait for init.
start_flow .

# Generate a file whose check spends many seconds inside a SINGLE optional-chain
# operation. Each `x?.f(arg)` is an optional *call* whose argument forces an
# 800-member object union (`A`) to flow against a disjoint 800-member parameter
# union (`P`). That subtyping does ~800*800 `FlowJs::flow` speculation attempts,
# all driven from inside `optional_chain::run` -- several seconds of work,
# polling `check_should_cancel` densely throughout.
#
# The operation must be this expensive *per call*, not merely high-volume: the
# cancel flag is sticky, so a worker churning through many *cheap* optional
# accesses almost always observes the flag at a statement boundary between them
# (where the cancel propagates cleanly via `?`) rather than mid-operation, and a
# plain `x?.p` spends its time in the cancel-safe tvar resolver rather than in a
# fallible concretization. Only a call that keeps the worker inside
# `optional_chain::run` for seconds reliably makes the poll that observes the
# cancel fire from *within* it. Several such calls back-to-back keep a worker
# mid-operation across the whole `sleep` window below, regardless of machine
# speed.
{
  printf '// @flow\n'
  printf 'type P = %s;\n' "$(seq 0 799 | sed 's/.*/{p&: &}/' | paste -sd '|' -)"
  printf 'type A = %s;\n' "$(seq 0 799 | sed 's/.*/{a&: &}/' | paste -sd '|' -)"
  printf 'declare var x: null | {| f: (arg: P) => void |};\n'
  printf 'declare var arg: A;\n'
  yes '(x?.f(arg));' | head -n 8
} > chain.js

# Kick off a recheck that hangs for 100s inside $Flow$DebugSleep (holding the
# "Checking files" phase open) while a sibling worker churns through chain.js.
cp sleep.js.ignored sleep.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js chain.js

# Give the sibling worker time to get deep into chain.js's optional-chain checks.
sleep 3

# Editing a dependent of the sleeping file cancels the in-flight recheck. On the
# buggy build, the chain.js worker observes WorkerCanceled inside the
# optional-chain path and panics; on a correct build the cancellation propagates
# and the recheck unwinds and rolls back cleanly.
echo " " >> sleep_dependent.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep_dependent.js

# Remove the sleeping file and let the recheck settle.
rm sleep.js sleep_dependent.js chain.js
assert_ok "$FLOW" force-recheck --no-auto-start sleep.js sleep_dependent.js chain.js

# The server must survive and report the one real error (foo.js).
assert_errors "$FLOW" status --no-auto-start --timeout 30

# Regression guard: the canceled recheck must not have panicked a worker.
printf "\\nWorker panicked on canceled recheck? "
grep -q "should not fail outside speculation\\|called \`Result::unwrap()\` on an \`Err\` value: WorkerCanceled" "$FLOW_LOG_FILE" \
  && echo "Yes (BUG)" \
  || echo "No"
