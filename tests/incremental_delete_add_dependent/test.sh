#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

rm foo.js
rm bar.js
mv baz.js.tmp baz.js

assert_ok "$FLOW" force-recheck --no-auto-start foo.js bar.js baz.js
assert_errors "$FLOW" status --no-auto-start

rm baz.js

assert_ok "$FLOW" force-recheck --no-auto-start baz.js
assert_ok "$FLOW" status --no-auto-start
