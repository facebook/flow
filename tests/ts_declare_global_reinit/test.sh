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

set_saved_state_changes() {
  : > .flow.saved_state_file_changes
  for file in "$@"; do
    printf "%s/%s\n" "$(pwd)" "$file" >> .flow.saved_state_file_changes
  done
}

assert_saved_state_rejected() {
  printf "\n\n======%s rejects saved state======\n" "$1"
  assert_exit 78 start_flow_unsafe . --saved-state-fetcher local --saved-state-no-fallback
  assert_ok "$FLOW" stop .
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

printf "\n\n======lazy saved-state load restores global augmentations======\n"
set_saved_state_changes
assert_ok "$FLOW" save-state --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop
start_flow . --lazy --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" force-recheck --focus consumer.js
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .

cp augmentation-number.ts.ignored augmentation.ts
set_saved_state_changes
assert_saved_state_rejected "an unreported changed global contributor"
cp augmentation-string.ts.ignored augmentation.ts

cp augmentation-number.ts.ignored augmentation.ts
set_saved_state_changes augmentation.ts
assert_saved_state_rejected "a changed global contributor"
cp augmentation-string.ts.ignored augmentation.ts

rm augmentation.ts
set_saved_state_changes augmentation.ts
assert_saved_state_rejected "a deleted global contributor"
cp augmentation-string.ts.ignored augmentation.ts

cp augmentation-string.ts.ignored created-augmentation.ts
set_saved_state_changes created-augmentation.ts
assert_saved_state_rejected "a new global contributor"
rm created-augmentation.ts

printf "\n\n======ordinary module change reuses saved state======\n"
cp augmentation-none.ts.ignored unrelated.ts
set_saved_state_changes unrelated.ts
start_flow . --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" status . > /dev/null
assert_ok "$FLOW" stop .
cp unrelated-after.ts.ignored unrelated.ts

set_saved_state_changes
start_flow . --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" status . > /dev/null

printf "\n\n======ordinary dependency edit stays incremental======\n"
before=$(reinit_count)
cp dependency-after.ts.ignored dependency.ts
assert_ok "$FLOW" force-recheck dependency.ts
assert_errors "$FLOW" status .
assert_no_reinit_since "$before"
echo "no full reinit"
