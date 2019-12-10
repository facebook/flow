#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" type-at-pos printBinaryExpression.js 15 12 --strip-root
assert_ok "$FLOW" type-at-pos printBinaryExpression.js 17 15 --strip-root
