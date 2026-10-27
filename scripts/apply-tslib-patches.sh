#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage: apply-tslib-patches.sh OUT_DIR
#
# Writes OUT_DIR/<name>.d.ts for each tslib_patches/<name>.diff: the library
# from tslib/ with the Flow-specific changes that the Flow binary applies.

set -e -o pipefail

OUT_DIR="$1"
ROOT="$(dirname "${BASH_SOURCE[0]}")/.."

rm -rf "${OUT_DIR:?}"
mkdir -p "$OUT_DIR"
for diff in "$ROOT"/rust_port/crates/flow_flowlib/src/tslib_patches/*.diff; do
  name="$(basename "$diff" .diff)"
  patch -s -F 0 -o "$OUT_DIR/$name.d.ts" "$ROOT/tslib/$name.d.ts" "$diff"
done
