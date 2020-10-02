#!/bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok \
  "$FLOW" type-at-pos test.js 5 12 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos importtest.js 5 12 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos importtest.js 9 12 --strip-root --pretty
