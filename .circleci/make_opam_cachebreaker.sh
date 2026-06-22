#!/bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

gcc -dumpfullversion -dumpversion
cat .circleci/config.yml
cat .github/workflows/build_and_test.yml
