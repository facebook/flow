#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

reinit_count() {
  awk 'index($0, "Will require full check reinit") { count++ } END { print count + 0 }' "$FLOW_LOG_FILE"
}

assert_reinit_since() {
  if [ "$1" -ge "$(reinit_count)" ]; then
    echo "expected full reinit"
    exit 1
  fi
}

assert_no_reinit_since() {
  if [ "$1" -ne "$(reinit_count)" ]; then
    echo "unexpected full reinit"
    exit 1
  fi
}

printf "======ordinary module has no global contribution======\n"
assert_errors "$FLOW" status .

printf "\n\n======adding the first block rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-string.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_ok "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======editing a contributor rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-number.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_errors "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======removing the block rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-none.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_errors "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======removing the module indicator rebuilds globals======\n"
cp augmentation-string.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_ok "$FLOW" status . > /dev/null
before=$(reinit_count)
cp augmentation-script.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_errors "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======restoring the module indicator rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-string.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_ok "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======a contributor parse failure rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-invalid.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_errors "$FLOW" status . > /dev/null
assert_reinit_since "$before"

printf "\n\n======recovering the contributor rebuilds globals======\n"
before=$(reinit_count)
cp augmentation-string.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_ok "$FLOW" status .
assert_reinit_since "$before"

printf "\n\n======renaming a contributor rebuilds globals======\n"
before=$(reinit_count)
mv augmentation.ts renamed-augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts renamed-augmentation.ts
assert_ok "$FLOW" status .
assert_reinit_since "$before"
mv renamed-augmentation.ts augmentation.ts
assert_ok "$FLOW" force-recheck renamed-augmentation.ts augmentation.ts
assert_ok "$FLOW" status . > /dev/null

printf "\n\n======deleting a contributor rebuilds globals======\n"
before=$(reinit_count)
rm augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_errors "$FLOW" status .
assert_reinit_since "$before"

cp augmentation-string.ts.ignored augmentation.ts
assert_ok "$FLOW" force-recheck augmentation.ts
assert_ok "$FLOW" status .

printf "\n\n======unrelated TypeScript edit stays incremental======\n"
before=$(reinit_count)
cp unrelated-after.ts.ignored unrelated.ts
assert_ok "$FLOW" force-recheck unrelated.ts
assert_ok "$FLOW" status . > /dev/null
assert_no_reinit_since "$before"
echo "no full reinit"

printf "\n\n======saved state restores global contributors======\n"
printf "\n" > .flow.saved_state_file_changes
assert_ok "$FLOW" save-state --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop
start_flow . --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" status .

printf "\n\n======ordinary dependency edit stays incremental======\n"
before=$(reinit_count)
cp dependency-after.ts.ignored dependency.ts
assert_ok "$FLOW" force-recheck dependency.ts
assert_errors "$FLOW" status .
assert_no_reinit_since "$before"
echo "no full reinit"
