#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" \
  "$FLOW" check --json pants

assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" \
  "$FLOW" check --pretty pants

assert_exit "$EXIT_USAGE" \
  "$FLOW" check --json --pants

assert_exit "$EXIT_USAGE" \
  "$FLOW" check --pretty --pants
