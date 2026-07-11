#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: file_not_modified
# Inverse of file_modified: checks that the agent left a file untouched.
# Used when the intended fix lives elsewhere (e.g. config/tooling evals where
# only the .flowconfig or a libdef should change) and editing the named file
# would be an incorrect, error-hiding solution.
#
# Usage: file_not_modified.sh <file> <original_file>
# Exit code: 0 = pass (file unchanged from original), 1 = fail (modified/deleted)

set -uo pipefail

FILE="$1"
ORIGINAL="$2"

if [ ! -f "$ORIGINAL" ]; then
  # No input-side original to compare against; nothing could have been modified.
  exit 0
fi

if [ ! -f "$FILE" ]; then
  echo "file_not_modified: file was deleted: $FILE" >&2
  exit 1
fi

if cmp -s "$FILE" "$ORIGINAL"; then
  exit 0
fi
echo "file_not_modified: file was modified: $FILE" >&2
exit 1
