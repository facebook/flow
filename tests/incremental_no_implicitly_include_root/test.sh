#!/bin/bash -e
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$FLOW" status .
assert_ok "$FLOW" force-recheck excluded.js
assert_errors "$FLOW" status .
