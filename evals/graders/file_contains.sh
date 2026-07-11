#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: file_contains
# Passes if <file> contains a line matching the extended-regex <pattern>.
# Used by config/tooling evals to assert a tooling construct is present
# (a FlowExpectedError suppression, a flowlint comment, an @flow strict pragma)
# since these are comments/pragmas rather than AST nodes.
#
# Usage: file_contains.sh <file> <pattern>
# Exit code: 0 = pass (pattern found), 1 = fail (not found / file missing)

set -uo pipefail

FILE="$1"
PATTERN="$2"

if [ ! -f "$FILE" ]; then
  echo "{\"pass\": false, \"reason\": \"file not found: $FILE\"}"
  exit 1
fi

if grep -Eq -- "$PATTERN" "$FILE"; then
  echo '{"pass": true}'
  exit 0
else
  echo "{\"pass\": false, \"reason\": \"pattern not found\"}"
  exit 1
fi
