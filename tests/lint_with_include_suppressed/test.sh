#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "Without --include-suppressed:\n"
assert_ok "$FLOW" check
printf "With --include-suppressed:\n"
assert_errors "$FLOW" check --include-suppressed
