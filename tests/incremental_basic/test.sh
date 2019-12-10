#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp ./*.js tmp/
assert_errors "$FLOW" status .
cp tmp1/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
cp tmp2/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
cp tmp3/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors "$FLOW" status .
mv tmp/*.js ./
rmdir tmp
