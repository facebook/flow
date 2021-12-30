#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" status --no-auto-start

# rename Dependency and make it slow, so we can interrupt the recheck
mv dependency.js dependency.js.ignored
mv dependency_slow.js.ignored dependency_slow.js

# start a very slow recheck
assert_ok "$FLOW" force-recheck dependency.js dependency_slow.js

sleep 2

# modify Dependency to cancel the recheck. we overwrite it with
# the fast version, so the test doesn't take forever.
mv dependency.js.ignored dependency_slow.js

assert_ok "$FLOW" force-recheck dependency_slow.js

assert_ok "$FLOW" status --no-auto-start
