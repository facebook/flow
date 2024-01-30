#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir link-src-tmp
ln -s link-src-tmp link-dest-tmp

printf "Default .flowconfig with bad code should not typecheck:\n"
assert_errors "$FLOW" check test.js --temp-dir link-dest-tmp/flow
printf "Default .flowconfig with bad code should not typecheck when run again with the same config:\n"
assert_errors "$FLOW" check test.js --temp-dir link-dest-tmp/flow
