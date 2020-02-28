#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nServer should start in fs lazy mode\\n"
start_flow . --lazy-mode fs

assert_ok "$FLOW" status --strip-root

echo

assert_ok "$FLOW" get-def --strip-root bar.js 5 2

assert_ok "$FLOW" stop
