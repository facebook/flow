#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# A saved state records which TypeScript declaration files were global libdefs. That set is
# only derivable from a whole-repo parse, so a delta that moves it makes the state unusable:
# the load refuses it -- exit 78 under --saved-state-no-fallback -- and the scratch check it
# falls back to must report exactly what a cold scratch run reports.
#
# A declaration file that is an ordinary module on both sides moves nothing global. Those
# still reuse the saved state, and passing --saved-state-no-fallback on the reuse runs is what
# proves they were not quietly answered by a scratch check.

changed() {
  : > .flow.saved_state_file_changes
  for file in "$@"; do
    printf "%s/%s\n" "$(pwd)" "$file" >> .flow.saved_state_file_changes
  done
}

cold_scratch() {
  printf "\n======cold scratch matches %s======\n" "$1"
  start_flow . --saved-state-fetcher none
  assert_errors "$FLOW" status .
  assert_ok "$FLOW" stop .
}

reused() {
  printf "\n\n======%s reuses saved state======\n" "$1"
  start_flow . --saved-state-fetcher local --saved-state-no-fallback
  assert_errors "$FLOW" status .
  assert_ok "$FLOW" stop .
  cold_scratch "$1"
}

rejected() {
  printf "\n\n======%s rejects saved state======\n" "$1"
  assert_exit 78 start_flow_unsafe . --saved-state-fetcher local --saved-state-no-fallback
  assert_ok "$FLOW" stop .
  printf "\n======%s falls back to a scratch check======\n" "$1"
  start_flow . --saved-state-fetcher local
  assert_errors "$FLOW" status .
  assert_ok "$FLOW" stop .
  cold_scratch "$1"
}

assert_ok "$FLOW" stop .

printf "======generate saved state======\n"
: > .flow.saved_state_file_changes
start_flow . --saved-state-fetcher none
assert_ok "$FLOW" save-state --root . --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop .

# Reuse: nothing in the delta can move the global scope.

changed use.js
reused "a changed source file"

# The four possible classification transitions for an existing declaration file. Cases 1-3 move
# or modify the global scope and must reject the saved state. Case 4 is an ordinary module update
# and must reuse it. The fallback and cold runs make the resulting semantic change visible.

cp promoted-global-template.d.ts.ignored saved-external.d.ts
changed saved-external.d.ts
rejected "case 1: non-global becomes global"
cp saved-external-template.d.ts.ignored saved-external.d.ts

cp external-global-template.d.ts.ignored global.d.ts
changed global.d.ts
rejected "case 2: global becomes non-global"
cp original-template.d.ts.ignored global.d.ts

cp valid-template.d.ts.ignored global.d.ts
changed global.d.ts
rejected "case 3: global remains global after content change"
cp original-template.d.ts.ignored global.d.ts

cp external-template.d.ts.ignored saved-external.d.ts
changed saved-external.d.ts
reused "case 4: non-global remains non-global after content change"
cp saved-external-template.d.ts.ignored saved-external.d.ts

# The saved state has no provider for this module at all, so every importer's saved
# resolution for it is "missing module". Reusing the state has to reach those importers
# through the ordinary recheck over the delta.
cp external-template.d.ts.ignored created-external.d.ts
changed created-external.d.ts
reused "a newly created external declaration"
rm created-external.d.ts

cp invalid-template.d.ts.ignored created-invalid.d.ts
changed created-invalid.d.ts
reused "a newly created invalid declaration"
rm created-invalid.d.ts

# Additional rejection cases: the delta names, removes, or invalidates a known global, or creates
# a new global that was absent from the saved state.

changed global.d.ts
rejected "a known global named by the delta"

cp invalid-template.d.ts.ignored global.d.ts
changed global.d.ts
rejected "a known global becoming invalid"
cp original-template.d.ts.ignored global.d.ts

mv global.d.ts global.d.ts.ignored
changed global.d.ts
rejected "a deleted known global"
mv global.d.ts.ignored global.d.ts

cp valid-template.d.ts.ignored renamed-global.d.ts
mv global.d.ts global.d.ts.ignored
changed global.d.ts renamed-global.d.ts
rejected "a renamed known global"
mv global.d.ts.ignored global.d.ts
rm renamed-global.d.ts

cp valid-template.d.ts.ignored created-global.d.ts
changed created-global.d.ts
rejected "a newly created global"

mv saved-external.d.ts saved-external.d.ts.ignored
changed created-global.d.ts saved-external.d.ts
rejected "a created global with a removed provider"
mv saved-external.d.ts.ignored saved-external.d.ts
rm created-global.d.ts

cp invalid-template.d.ts.ignored global.d.ts
mv saved-external.d.ts saved-external.d.ts.ignored
changed global.d.ts saved-external.d.ts
rejected "an invalid global with a removed provider"
mv saved-external.d.ts.ignored saved-external.d.ts
cp original-template.d.ts.ignored global.d.ts

# Lazy init takes the same two paths.

printf "\n\n======lazy external declaration reuses saved state======\n"
cp external-template.d.ts.ignored created-external.d.ts
changed created-external.d.ts
start_flow . --lazy --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" force-recheck --focus use.js
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .

printf "\n======cold scratch lazy matches external declaration======\n"
start_flow . --lazy --saved-state-fetcher none
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" force-recheck --focus use.js
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .
rm created-external.d.ts

printf "\n\n======lazy invalid declaration reuses saved state======\n"
cp invalid-template.d.ts.ignored created-invalid.d.ts
changed created-invalid.d.ts
start_flow . --lazy --saved-state-fetcher local --saved-state-no-fallback
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .

printf "\n======cold scratch lazy matches invalid declaration======\n"
start_flow . --lazy --saved-state-fetcher none
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .
rm created-invalid.d.ts

printf "\n\n======lazy created global rejects saved state======\n"
cp valid-template.d.ts.ignored created-global.d.ts
changed created-global.d.ts
assert_exit 78 start_flow_unsafe . --lazy \
  --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" stop .

printf "\n======lazy created global falls back to a scratch check======\n"
start_flow . --lazy --saved-state-fetcher local
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" force-recheck --focus use.js
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .

printf "\n======cold scratch lazy matches created global======\n"
start_flow . --lazy --saved-state-fetcher none
assert_ok "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" force-recheck --focus use.js
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .
rm created-global.d.ts

printf "\n\n======lazy changed known global rejects saved state======\n"
cp invalid-template.d.ts.ignored global.d.ts
changed global.d.ts
assert_exit 78 start_flow_unsafe . --lazy \
  --saved-state-fetcher local --saved-state-no-fallback
assert_ok "$FLOW" stop .

printf "\n======lazy changed known global falls back to a scratch check======\n"
start_flow . --lazy --saved-state-fetcher local
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .

printf "\n======cold scratch lazy matches changed known global======\n"
start_flow . --lazy --saved-state-fetcher none
assert_errors "$FLOW" status --show-lazy-status .
assert_ok "$FLOW" stop .
cp original-template.d.ts.ignored global.d.ts

# A file the saved state could not classify is not one of its globals, so fixing it into one
# moves the global scope just as much as breaking one does. This regenerates the saved state,
# so it runs after everything above.
printf "\n\n======regenerate saved state with an invalid global======\n"
cp invalid-template.d.ts.ignored global.d.ts
: > .flow.saved_state_file_changes
start_flow . --saved-state-fetcher none
assert_ok "$FLOW" save-state --root . --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop .
cp original-template.d.ts.ignored global.d.ts
changed global.d.ts
rejected "a fixed global"

printf "\n\n======declaration-support option mismatch rejects saved state======\n"
sed -i'.orig' -e \
  's/experimental.typescript_library_definition_support=true/experimental.typescript_library_definition_support=false/' \
  .flowconfig
changed .flowconfig
assert_exit 78 start_flow_unsafe . \
  --saved-state-fetcher local --saved-state-no-fallback
printf "\n"
