#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Invalid flowconfig should fail"
assert_exit 8 "$FLOW" check 2>&1

echo
echo "Invalid flowconfig should pass"
assert_ok "$FLOW" check --quiet --ignore-version 2>&1

echo
echo "flow config check:"
assert_exit 8 "$FLOW" config check --pretty

echo
echo "flow config check --ignore-version:"
assert_ok "$FLOW" config check --ignore-version --pretty
