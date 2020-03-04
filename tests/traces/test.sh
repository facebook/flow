#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_errors "$FLOW" check --traces 1 .
assert_errors "$FLOW" check --traces 3 .
assert_errors "$FLOW" check --traces 5 .
