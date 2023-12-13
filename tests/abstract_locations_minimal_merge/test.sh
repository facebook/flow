#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" status --strip-root

# We should drive merge based on the sig dependency graph, not the implementation dependency graph.
# Therefore, only `foo.js` and `sig-dependent.js` should be considered for merge.
#
# Currently, we also consider `impl-dependent.js` and `impl-dep-of-sig-dep.js`, because we need to
# check these files and having the to-check set be a subset of the to-merge is simpler, and merging
# is not expensive anyway.

printf "\\nChanging the exported type should produce downstream errors.\\n"
printf "We should recheck all files, but should not remerge files that are not recursive sig "
printf "dependents.\\n"
sed -i -e 's/string/number/' foo.js
assert_ok "$FLOW" force-recheck foo.js
assert_errors "$FLOW" status --strip-root
show_skipping_stats "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop
