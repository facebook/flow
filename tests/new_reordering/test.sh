#! /bin/sh
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


echo "Typechecking for cycles..."
assert_errors "$FLOW" status

echo "Annotating cycles..."
"$FLOW" codemod annotate-cycles . --strip-root
