#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: ast_query (library / meta-grader)
# Runs a jq selector against the full AST of a Flow file.
# Uses `flow ast` to parse the file, then searches all objects in the AST
# tree with the given jq selector expression.
#
# The selector is wrapped in: jq "[.. | objects | select(<selector>)] | length"
#
# Usage: ast_query.sh <file> <flow_bin> <jq_selector> [--negate]
# Exit code: 0 = pass, 1 = fail
#
# Examples:
#   ast_query.sh main.js ./flow '.type == "MatchExpression"'
#   ast_query.sh main.js ./flow '.type == "AnyTypeAnnotation"' --negate
#   ast_query.sh main.js ./flow '.type == "TypeOperator" and .operator == "renders"'

set -euo pipefail

FILE="$1"
FLOW_BIN="$2"
SELECTOR="$3"
NEGATE="${4:-}"

if ! [[ -f "$FILE" ]]; then
  echo '{"pass": false, "reason": "file not found"}'
  exit 1
fi

# Parse the file with flow ast. If parsing fails entirely (non-JSON output),
# the jq pipeline will produce no matches, which we handle gracefully.
AST=$("$FLOW_BIN" ast "$FILE" 2>/dev/null || true)

# Search recursively for any object matching the selector
MATCH_COUNT=$(echo "$AST" | jq "[.. | objects | select($SELECTOR)] | length" 2>/dev/null || echo 0)

if [[ "$NEGATE" == "--negate" ]]; then
  if [[ "$MATCH_COUNT" -gt 0 ]]; then
    echo "{\"pass\": false, \"selector\": $(echo "$SELECTOR" | jq -Rs .), \"negate\": true, \"reason\": \"match found in AST (should be absent)\"}"
    exit 1
  else
    echo "{\"pass\": true, \"selector\": $(echo "$SELECTOR" | jq -Rs .), \"negate\": true, \"desc\": \"correctly absent from AST\"}"
    exit 0
  fi
else
  if [[ "$MATCH_COUNT" -gt 0 ]]; then
    echo "{\"pass\": true, \"selector\": $(echo "$SELECTOR" | jq -Rs .), \"desc\": \"match found in AST ($MATCH_COUNT occurrence(s))\"}"
    exit 0
  else
    echo "{\"pass\": false, \"selector\": $(echo "$SELECTOR" | jq -Rs .), \"reason\": \"no match found in AST\"}"
    exit 1
  fi
fi
