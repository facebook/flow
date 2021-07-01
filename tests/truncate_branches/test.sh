#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$1" check . --no-flowlib
assert_errors "$1" check . --no-flowlib --show-all-branches
