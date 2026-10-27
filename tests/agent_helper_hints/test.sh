#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "===== status: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" status --no-auto-start

printf "\\n===== full-check: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" full-check .

printf "\\n===== check-contents: deprecated syntax points at unpack-docs =====\\n"
assert_errors "$FLOW" check-contents < a.js

printf "\\n===== check-contents: other errors get no hint =====\\n"
printf "const y: string = 1;\\n" | assert_errors "$FLOW" check-contents
