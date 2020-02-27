#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Be careful: because we don't start a new Flow server for each request, the
# impact of one request here on the server's lazy mode state can impact the
# results of other requests. Reorder with care.

echo "finding global references for a class"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root foo.js 3 14
