#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: no_any
# Checks that the output does not use `any` type annotations.
# Thin wrapper around ast_query.sh with --negate.
#
# Usage: no_any.sh <file> <flow_bin>
# Exit code: 0 = pass, 1 = fail

set -euo pipefail

GRADERS_DIR="$(cd "$(dirname "$0")" && pwd)"

FILE="$1"
FLOW_BIN="$2"

exec bash "$GRADERS_DIR/ast_query.sh" "$FILE" "$FLOW_BIN" \
  '.type == "AnyTypeAnnotation"' \
  --negate
