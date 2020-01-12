#!/bin/bash -e
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" force-recheck --no-auto-start a.js
assert_ok "$FLOW" status --no-auto-start .

cp tmp/b.js ./
trap 'rm b.js' EXIT

assert_ok "$FLOW" force-recheck --no-auto-start b.js
assert_errors "$FLOW" status --no-auto-start .
