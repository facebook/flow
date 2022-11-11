#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "'package' module resolves to file:\n"
assert_errors "$FLOW" status
echo

printf "'package' module resolves to package:\n"
mv fixtures/package package
mv package.js fixtures/package.js
assert_ok "$FLOW" force-recheck package/package.json package/index.js package.js
assert_errors "$FLOW" status
echo

printf "'package' module resolves to file again:\n"
mv package fixtures/package
mv fixtures/package.js package.js
assert_ok "$FLOW" force-recheck package/package.json package/index.js package.js
assert_errors "$FLOW" status
