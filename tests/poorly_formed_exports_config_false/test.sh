#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "FLOW STATUS:"

assert_ok $FLOW status

echo "FLOW CHECK-CONTENTS:"

assert_ok $FLOW check-contents bad-default-export.js < bad-default-export.js
