#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "common_test.js" --pretty --imports
queries_in_file autocomplete "foo/test.js" --pretty --imports
queries_in_file autocomplete "bar/test.js" --pretty --imports
