#! /bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

echo
echo "exact_by_default=false (should print: {|f: number|} for exact)"
echo
assert_ok "$FLOW" codemod annotate-exports --strip-root .

echo
echo "exact_by_default=true (should print: {f: number, ...} for inexact)"
echo
echo "exact_by_default=true" >> .flowconfig
assert_ok "$FLOW" codemod annotate-exports --strip-root .
