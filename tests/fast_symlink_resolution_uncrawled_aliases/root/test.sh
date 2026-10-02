#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "====== fast symlink resolution enabled ======\n"
assert_errors "$FLOW" status .
assert_ok "$FLOW" stop .

printf "\n====== fast symlink resolution disabled ======\n"
sed 's/fast_symlink_resolution=true/fast_symlink_resolution=false/' .flowconfig > .flowconfig.disabled
mv .flowconfig.disabled .flowconfig
assert_errors "$FLOW" status .
