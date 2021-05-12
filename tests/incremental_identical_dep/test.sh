# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$FLOW" status

# We change baz's re-export from ./foo to an identical ./bar, which causes qux's
# re-export to change, which affects the error printed in thud, since it now
# needs to point to bar.js instead of foo.js
mv baz.js.after baz.js
assert_ok "$FLOW" force-recheck baz.js

assert_errors "$FLOW" status
