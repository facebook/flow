#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test that find-module resolves haste modules when the filename is a relative
# path. Before the expand_path fix, the relative path would not match any
# project path mapping, causing haste resolution to be skipped.
assert_ok "$FLOW" find-module --strip-root --json Provider web/consumer.js
