#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#
# Grader: contains_ast_node_type
# Checks that the output contains (or does not contain) specific AST node types.
# Thin wrapper around ast_query.sh with selector: .type == "$QUERY"
#
# Usage: contains_ast_node_type.sh <file> <flow_bin> <node_type> [--negate]
# Exit code: 0 = pass, 1 = fail
#
# Examples:
#   contains_ast_node_type.sh main.js ./flow ComponentDeclaration
#   contains_ast_node_type.sh main.js ./flow SwitchStatement --negate

set -euo pipefail

GRADERS_DIR="$(cd "$(dirname "$0")" && pwd)"

FILE="$1"
FLOW_BIN="$2"
QUERY="$3"
NEGATE="${4:-}"

exec bash "$GRADERS_DIR/ast_query.sh" "$FILE" "$FLOW_BIN" ".type == \"$QUERY\"" $NEGATE
