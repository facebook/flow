#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp ./*.js tmp/

printf "Initial status:\n"
assert_errors $FLOW status --no-auto-start --strip-root .

printf "\nChange .js file:\n"
cp tmp1/*.js ./
assert_ok "$FLOW" force-recheck ./*.js # overapproximation
assert_errors $FLOW status --no-auto-start --strip-root .

mv tmp/*.js ./
rmdir tmp
