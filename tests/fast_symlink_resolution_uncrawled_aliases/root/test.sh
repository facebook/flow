#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "====== fast symlink resolution enabled ======\n"
export FLOW_FAST_SYMLINK_RESOLUTION=true
assert_errors "$FLOW" status .
assert_ok "$FLOW" stop .

printf "\n====== fast symlink resolution disabled ======\n"
export FLOW_FAST_SYMLINK_RESOLUTION=false
assert_errors "$FLOW" status .
