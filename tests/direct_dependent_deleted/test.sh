#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" status

# make a change to dependency.js, to populate its direct dependents cache
printf "\n(123: number);\n" >> dependency.js
assert_ok "$FLOW" force-recheck dependency.js

# delete dependent.js
rm dependent.js
assert_ok "$FLOW" force-recheck dependent.js

# make another change to dependency.js. if this hits the direct dependents cache
# it will think dependent is still there, and blow up.
printf "\n(123: number);\n" >> dependency.js
assert_ok "$FLOW" force-recheck dependency.js

assert_ok "$FLOW" status --no-auto-start
