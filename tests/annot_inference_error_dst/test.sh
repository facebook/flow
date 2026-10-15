#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# dep_a.js <- dep_b.js <- {user1.js, user2.js}
#
# The `invalid-exported-annotation` error in dep_a.js is raised by annotation
# inference in whichever checked file forces `P`. user1.js forces dep_b's
# `A` namespace binding (merging dep_a) but not `P`; user2.js then forces `P`
# through that same cached binding.

printf "\nFocus-check user2.js alone reports the error in dep_a.js:\n"
assert_errors "$FLOW" focus-check --max-workers 1 user2.js

# BUG: checking user1.js first in the same batch leaves dep_a's merge_dst_cx
# pointing at user1.js, so the error raised while checking user2.js is lost.
# This should report the same error as above.
printf "\nFocus-check user1.js and user2.js in one batch:\n"
assert_ok "$FLOW" focus-check --max-workers 1 user1.js user2.js

printf "\nDone!\n"
