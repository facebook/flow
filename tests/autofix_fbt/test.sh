#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm -r tmp/*
cp -r .flowconfig lib tmp
start_flow tmp

cp a.js tmp/a.js
assert_ok "$FLOW" autofix insert-type --in-place a.js 6 18 6 23
assert_ok "$FLOW" force-recheck a.js
echo "> cat a.js"
cat a.js

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
