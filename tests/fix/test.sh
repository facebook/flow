#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "flow fix\n"
assert_ok "$FLOW" fix --strip-root
printf "flow fix --error-codes method-unbinding\n"
assert_ok "$FLOW" fix --error-codes method-unbinding --strip-root
