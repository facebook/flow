#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nFlowconfig should not be valid:\n";
assert_exit 8 "$FLOW" check --strip-root --no-flowlib . 2>&1
