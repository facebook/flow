#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

# focus dependency, which checks cycle_a and cycle_b as dependents
assert_ok "$FLOW" force-recheck --focus dependency.js

# finds an error in cycle_b (and 1 is suppressed)
assert_errors "$FLOW" status

# change dependency's signature, which causes cycle_a and cycle_b to
# recheck. cycle_a is a direct dependent so it gets both merged and
# checked, but since the sig change was irrelevant, cycle_b gets
# merged but not checked due to a recheck opt.
printf "\nexport const newthing : string = 'new';" >> dependency.js

assert_ok "$FLOW" force-recheck dependency.js

# asserts that the errors in cycle_b are still returned.
assert_errors "$FLOW" status
