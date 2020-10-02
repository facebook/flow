#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" status --strip-root

# We should drive merge based on the sig dependency graph, not the implementation dependency graph.
# Therefore, only `foo.js` and `sig-dependent.js` should be considered for merge. At the time that
# this test is being written, we consider all six files for merge. This results in unnecessary work.

printf "\\nChanging the exported type should produce downstream errors.\\n"
printf "We should recheck all files, but should not remerge files that are not recursive sig "
printf "dependents.\\n"
sed -i -e 's/string/number/' foo.js
assert_ok "$FLOW" force-recheck foo.js
assert_errors "$FLOW" status --strip-root
show_skipping_stats_types_first "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop
