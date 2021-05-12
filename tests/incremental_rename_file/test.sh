# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$FLOW" status

# We move a.js, which provides HasteName, to a different path, which causes b.js
# to be rechecked, but we also need to recheck c.js because the error found in c
# now needs to point to bar/a.js instead of foo/a.js
mv foo/a.js bar/a.js
assert_ok "$FLOW" force-recheck foo/a.js bar/a.js

assert_errors "$FLOW" status
