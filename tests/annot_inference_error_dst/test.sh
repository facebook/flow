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

# Checking user1.js first in the same batch must not keep dep_a reporting to
# user1.js after its check is over.
printf "\nFocus-check user1.js and user2.js in one batch reports the same error:\n"
assert_errors "$FLOW" focus-check --max-workers 1 user1.js user2.js

printf "\nDone!\n"
