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
"$FLOW" glean --output-dir test1 --write-root test1 test.js
"$FLOW" glean --output-dir test2 --write-root test2 --saved-state-fetcher local --saved-state-no-fallback test.js
