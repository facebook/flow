#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

show_queries() {
  printf "Type at global use:\n"
  assert_ok "$FLOW" type-at-pos --strip-root use.js 1 18
  printf "Definition of global use:\n"
  assert_ok "$FLOW" get-def --strip-root use.js 1 18
  printf "Autocomplete at global prefix:\n"
  queries_in_file autocomplete "autocomplete.js" --pretty
  printf "Autoimport at declaration prefix:\n"
  queries_in_file autocomplete "autoimport.js" --pretty --imports
}

printf "======Queries before the global exists======\n"
assert_errors "$FLOW" status .
show_queries

printf "\n\n======Queries after the global is created======\n"
cp global.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .
show_queries

printf "\n\n======Queries after the global becomes external======\n"
cp external.d.ts.ignored live.d.ts
assert_ok "$FLOW" force-recheck live.d.ts
assert_errors "$FLOW" status .
show_queries
