#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
cp a.js tmp/a.js

start_flow tmp

assert_ok "$FLOW" apply-code-action 'source.addMissingImports' tmp/a.js
