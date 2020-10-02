#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop 2> /dev/null > /dev/null
for i in $(seq 1 20); do
  assert_ok "$FLOW" status 2> /dev/null > /dev/null &
done
assert_ok "$FLOW" status
