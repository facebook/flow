#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop

echo
echo '=== Okay to print any type'
echo
assert_ok "$FLOW" codemod annotate-exports --strip-root .

echo
echo "=== Should print \$MyPreferedSuppression instead of the any type"
echo
assert_ok "$FLOW" codemod annotate-exports --strip-root --lints unclear-type=error .
