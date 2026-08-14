#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

# Merging a checked dependency puts an entry holding the request's transaction
# into the check-contents cache.
assert_ok "$FLOW" check-contents --no-auto-start src/importer.js < src/importer.js

cp importer_modified.js.ignored src/importer.js
assert_ok "$FLOW" force-recheck --no-auto-start "$PWD/src/importer.js"
assert_ok "$FLOW" force-recheck --no-auto-start --focus src/importer.js

# This times out if the recheck's commit is stuck behind the reader that
# check-contents left behind.
assert_ok "$FLOW" status --no-auto-start --strip-root --timeout 60
