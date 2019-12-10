#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nCheck without --all doesn't read malformed @flow file\n"
assert_ok "$FLOW" check --strip-root .

printf "\nCheck with --all sees a parse error\n"
assert_errors "$FLOW" check --all --strip-root .
