#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "get-def" "requires.js" --strip-root --pretty
queries_in_file "get-def" "types.js" --strip-root --pretty
