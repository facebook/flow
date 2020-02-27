#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nErrors in classic\\n"
assert_errors "$FLOW" check --strip-root --no-flowlib

printf "\\nErrors in types-first\\n"
assert_errors "$FLOW" check --strip-root --no-flowlib --types-first
