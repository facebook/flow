#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "" > .flow.saved_state_file_changes

"$FLOW" stop
"$FLOW" start --saved-state-fetcher none
"$FLOW" save-state --out .flow.saved_state >> /dev/null
"$FLOW" stop
mkdir test1
mkdir test2
mkdir test3
mkdir test4
echo "Indexing 1 file without saved state"
"$FLOW" glean --output-dir test1 --write-root test1 test.js
echo "Indexing 1 file with saved state"
"$FLOW" glean --output-dir test2 --write-root test2 --saved-state-fetcher local --saved-state-no-fallback test.js
echo "Indexing 1 file + its direct deps (a.js, re-export.js) with saved state"
"$FLOW" glean --output-dir test3 --write-root test3 --saved-state-fetcher local --saved-state-no-fallback --include-direct-deps test.js
echo "Indexing 1 file + its pruned transitive deps (a.js, b.js, re-export.js) with saved state"
"$FLOW" glean --output-dir test4 --write-root test4 --saved-state-fetcher local --saved-state-no-fallback --include-reachable-deps test.js
