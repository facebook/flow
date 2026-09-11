#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$FLOW" status --strip-root

printf '\nCustom global Symbol libdef:\n'
assert_errors "$FLOW" full-check libdef --strip-root
