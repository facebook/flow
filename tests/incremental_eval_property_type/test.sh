#! /bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" status

cp a.js.after a.js

assert_ok "$FLOW" force-recheck --no-auto-start a.js b.js

assert_errors "$FLOW" status
