#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# runtests.sh [ARG]... is a shortcut for the Rust test runner:
#   facebook/flowd dev-tools runtests [ARG]...
# Run it from fbcode/flow.
export IN_FLOW_TEST=1
exec facebook/flowd dev-tools runtests "$@"
