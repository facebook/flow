#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

printf "==== No errors at start====\\n"
assert_ok "$FLOW" status --no-auto-start

# simulate a file watcher race condition by changing dependency.js without
# a force-recheck.
printf "\\n\\nexport const y = 123;\\n" >> dependency.js

# checking file.js needs dependency.js, and we notice it's changed. we have
# to re-merge it, but check-contents doesn't wait for dependent.js to be checked.
printf "\\n==== Checking file.js finds errors ====\\n"
assert_errors "$FLOW" check-contents --strip-root file.js < file.js
# 1 file should be merged (dependency) and nothing should be checked
show_skipping_stats "$FLOW_LOG_FILE"

# here's the late file watcher update
assert_ok "$FLOW" force-recheck dependency.js

# even though we've already noticed the update, now is when we actually
# recheck dependency.js and its dependents, and find the error in
# dependent.js
printf "\\n==== force-recheck checks dependent ====\\n"
assert_errors "$FLOW" status --no-auto-start
# 3 files (dependency.js and its dependents, dependent.js and file.js) should
# be both merged and checked
show_skipping_stats "$FLOW_LOG_FILE"
