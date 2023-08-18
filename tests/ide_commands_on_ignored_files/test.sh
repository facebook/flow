#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "get def should not work for ignored files\n"
assert_ok "$FLOW" get-def ignore/foo.js 3 2 --strip-root --pretty
printf "type-at-pos should not work for ignored files\n"
assert_ok "$FLOW" type-at-pos ignore/foo.js 3 2 --strip-root --pretty

printf "get def should not work for noflow files\n"
assert_ok "$FLOW" get-def no_flow/foo.js 3 2 --strip-root --pretty
printf "type-at-pos should not work for noflow files\n"
assert_ok "$FLOW" type-at-pos no_flow/foo.js 3 2 --strip-root --pretty
