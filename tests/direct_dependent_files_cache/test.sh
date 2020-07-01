#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

log_file="$FLOW_TEMP_DIR/direct_dependent_files_cache.log"

start_flow src

printf "== Initial there should be 2 errors ==\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Delete unrelated.js and now there is 1 error ==\n"
# Unchanged during `ResolvedRequires` (we fail to delete them)
# Unchanged during `ReresolveDirectDependents`
assert_ok mv src/unrelated{.js,.js.ignored}
assert_ok "$FLOW" force-recheck --profile src/unrelated.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Restore unrelated.js and back to 2 errors ==\n"
# Unchanged during `ResolvedRequires` (we forget to delete them so still there)
# Unchanged during `ReresolveDirectDependents`
assert_ok mv src/unrelated{.js.ignored,.js}
assert_ok "$FLOW" force-recheck --profile src/unrelated.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Delete src/node_modules/dependency.js changes an error ==\n"
# Unchanged during `ResolvedRequires` (we fail to delete them)
# Changed during `ReresolveDirectDependents`
assert_ok mv src/node_modules/dependency{.js,.js.ignored}
assert_ok "$FLOW" force-recheck --profile src/node_modules/dependency.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Restore src/node_modules/dependency.js change it back ==\n"
# Unchanged during `ResolvedRequires` (we forget to delete them so still there)
# Changed during `ReresolveDirectDependents`
assert_ok mv src/node_modules/dependency{.js.ignored,.js}
assert_ok "$FLOW" force-recheck --profile src/node_modules/dependency.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Remove the import from dependent.js ==\n"
# Changed during `ResolvedRequires`
# Unchanged during `ReresolveDirectDependents`
assert_ok mv src/dependent{.js,.js.ignored}
assert_ok echo "// @flow" > src/dependent.js
assert_ok "$FLOW" force-recheck --profile src/dependent.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Add the import back to dependent.js ==\n"
# Changed during `ResolvedRequires`
# Unchanged during `ReresolveDirectDependents`
assert_ok mv src/dependent{.js.ignored,.js}
assert_ok "$FLOW" force-recheck --profile src/dependent.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

printf "\n== Adding code that doesn't import has no effect on dep graph ==\n"
# Unchanged during `ResolvedRequires`
# Unchanged during `ReresolveDirectDependents`
assert_ok echo "export var foo: bool = 123" >> src/node_modules/dependency.js
assert_ok "$FLOW" force-recheck --profile src/node_modules/dependency.js \
  > /dev/null

grep "Resolved requires" "$log_file" | tail -n 2 | cut -d"]" -f 2
printf "\n"
assert_errors "$FLOW" status --no-auto-start src

"$FLOW" stop src 1> /dev/null 2>&1
