#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

printf "Creating git repo\n"
git init . --quiet
git config user.email "flow@example.com"
git config user.name "Flow"
echo ".flow.saved_states" > .gitignore
echo

printf "Creating initial commit\n"
git add .gitignore .flowconfig test.js dep.js
git commit -m "initial commit" --quiet
mkdir -p .flow.saved_states
echo

printf "Creating saved state\n"
COMMIT1="$(git rev-parse HEAD)"
TIME1="$(git log --format=%ct -n1 HEAD)"
assert_ok create_saved_state . ".flowconfig"
mv .flow.saved_state ".flow.saved_states/${TIME1}_${COMMIT1}"
rm .flow.saved_state_file_changes
echo

printf "Creating commit 2\n"
cp dep.js dep2.js
git add dep2.js
git commit -m "add dep2" --quiet
echo

# Make sure timestamps aren't the same
sleep 1

printf "Creating second saved state\n"
COMMIT2="$(git rev-parse HEAD)"
TIME2="$(git log --format=%ct -n1 HEAD)"
assert_ok create_saved_state . ".flowconfig"
mv .flow.saved_state ".flow.saved_states/${TIME2}_${COMMIT2}"
rm .flow.saved_state_file_changes
echo

printf "Moving back to initial commit\n"
git checkout "$COMMIT1" --quiet
echo

assert_ok "$FLOW" start \
  --file-watcher=none \
  --saved-state-fetcher=scm \
  --saved-state-no-fallback \
  --wait

# --saved-state-no-fallback doesn't error if there's no saved state found
# so make sure we found the correct one.
printf "Found correct merge base hash: "
grep -q "Saved state merge base hash is \"$COMMIT1\"" "$FLOW_LOG_FILE" && echo "Yes" || echo "No"
echo

assert_ok "$FLOW" stop

# Move to commit2 with dep2
printf "Moving to commit 2\n"
git checkout -f "$COMMIT2" --quiet
echo

assert_ok "$FLOW" start \
  --file-watcher=none \
  --saved-state-fetcher=scm \
  --saved-state-no-fallback \
  --wait

printf "Found correct merge base hash: "
grep -q "Saved state merge base hash is \"$COMMIT2\"" "$FLOW_LOG_FILE" && echo "Yes" || echo "No"

assert_ok "$FLOW" stop
