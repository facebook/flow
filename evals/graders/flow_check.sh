#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: flow_check
# Runs `flow check` on the eval output directory and returns pass/fail.
#
# Usage: flow_check.sh <eval_dir> [flow_bin]
# Exit code: 0 = pass (no errors), 1 = fail (errors found)
# Stdout: JSON with {pass, error_count, errors}

set -euo pipefail

EVAL_DIR="$1"
FLOW_BIN="${2:-flow}"

# Stop any running Flow server to avoid conflicts with servers started during eval
"$FLOW_BIN" stop "$EVAL_DIR" 2>/dev/null || true

# Run flow check
FLOW_EXIT=0
OUTPUT=$("$FLOW_BIN" full-check --strip-root --json "$EVAL_DIR" 2>/dev/null) || FLOW_EXIT=$?

# Detect Flow infrastructure failures (bad .flowconfig, server crash, etc.)
# Flow returns exit code 8 for invalid .flowconfig and outputs JSON without an "errors" key.
HAS_ERRORS=$(echo "$OUTPUT" | jq 'has("errors")' 2>/dev/null || echo "false")
if [[ "$HAS_ERRORS" != "true" ]]; then
  MSG=$(echo "$OUTPUT" | jq -r '.exit.msg // empty' 2>/dev/null) || MSG="Flow failed (exit $FLOW_EXIT)"
  jq -n --arg msg "$MSG" '{"pass": false, "error_count": -1, "errors": [{"message": $msg}]}'
  exit 1
fi

ERROR_COUNT=$(echo "$OUTPUT" | jq -r '.errors | length' 2>/dev/null || echo "-1")

if [[ "$ERROR_COUNT" == "0" ]]; then
  echo '{"pass": true, "error_count": 0, "errors": []}'
  exit 0
else
  ERRORS=$(echo "$OUTPUT" | jq -c '.errors // []' 2>/dev/null || echo '[]')
  echo "{\"pass\": false, \"error_count\": $ERROR_COUNT, \"errors\": $ERRORS}"
  exit 1
fi
