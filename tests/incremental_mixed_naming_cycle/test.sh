#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp root.js tmp/

assert_errors "$FLOW" status .
cp tmp1/root.js ./
assert_ok "$FLOW" force-recheck root.js
assert_errors "$FLOW" status .

mv tmp/root.js ./
rmdir tmp
