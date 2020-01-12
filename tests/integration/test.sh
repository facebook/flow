#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mv bar.js _bar.js
assert_ok $FLOW force-recheck --root . bar.js _bar.js
assert_errors $FLOW status .
mv _bar.js bar.js
