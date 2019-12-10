#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nNo errors:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root

printf "\nFocusing the unparsed file shouldn't blow up:\n"
assert_ok "$FLOW" force-recheck --focus --no-auto-start unparsed.js

printf "\nNo errors:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root
