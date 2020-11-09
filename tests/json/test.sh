#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

assert_errors "$FLOW" check --no-flowlib .

echo "{" > package.json
echo
echo "============================================"
echo "Parse error in package.json will be reported"
echo "============================================"
assert_errors "$FLOW" check --no-flowlib .
