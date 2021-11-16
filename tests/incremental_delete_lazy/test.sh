#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# server is auto-started

rm b.js

assert_ok "$FLOW" force-recheck --focus b.js

assert_errors "$FLOW" status --strip-root --no-auto-start
