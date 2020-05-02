#! /bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo
echo "exact_by_default=false"
echo
queries_in_file "type-at-pos" "file.js"

assert_ok "$FLOW" stop
echo
echo "exact_by_default=true"
echo
echo "exact_by_default=true" >> .flowconfig
queries_in_file "type-at-pos" "file.js"
