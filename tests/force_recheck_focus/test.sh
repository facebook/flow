#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "===== force-recheck of a unchanged file does nothing: =====\\n"
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start

printf "\\n===== force-recheck --focus of a unchanged file does a recheck: =====\\n"
assert_ok "$FLOW" force-recheck --focus a.js
assert_errors "$FLOW" status --no-auto-start
