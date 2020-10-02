#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Hopefully this will be short enough not to upset windows CI
LONG_PATH="really_long_path/really_long_path/really_long_path/really_long_path/\
really_long_path/really_long_path/really_long_path/really_long_path/\
really_long_path/really_long_path/really_long_path"

printf "\nFlow check:\n"
assert_errors "$FLOW" check --strip-root "$LONG_PATH"

printf "\nFlow status:\n"
start_flow "$LONG_PATH"
assert_errors "$FLOW" status --strip-root --no-auto-start "$LONG_PATH"
assert_ok "$FLOW" stop "$LONG_PATH"
