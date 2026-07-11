#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: file_modified
# Checks that the agent actually modified the file from its original input.
# Compares <file> against the stashed original at .eval_original/<relpath>.
#
# Usage: file_modified.sh <file> <original_file>
# Exit code: 0 = pass (file differs from original), 1 = fail (unchanged or missing)

set -uo pipefail

FILE="$1"
ORIGINAL="$2"

if [ ! -f "$ORIGINAL" ]; then
  # No input-side original means this eval doesn't use this file (multi-file eval).
  # The agent creates new files instead; flow_check and AST graders cover correctness.
  exit 0
fi

if [ ! -f "$FILE" ]; then
  echo "file_modified: file not found: $FILE" >&2
  exit 1
fi

if cmp -s "$FILE" "$ORIGINAL"; then
  exit 1
fi
exit 0
