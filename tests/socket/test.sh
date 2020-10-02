#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

LONG="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
mkdir "$LONG"
touch "$LONG/.flowconfig"
assert_ok "$FLOW" start "$LONG"
assert_ok "$FLOW" stop "$LONG"

mkdir -p "tmp/$LONG"
TMPDIR="tmp/$LONG"
export TMPDIR
FLOW_TEMP_DIR="$TMPDIR"
export FLOW_TEMP_DIR
assert_ok "$FLOW" start "$LONG"
assert_ok "$FLOW" stop "$LONG"
