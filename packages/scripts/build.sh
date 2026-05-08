#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Workspace-root build for the oxidized Flow packages. Mirrors upstream
# xplat/static_h/tools/hermes-parser/js/scripts/build.sh in shape: yarn install,
# then for each package, populate dist/ from src/ via the cp/rename/babel
# pattern, then build and embed the WASM parser into flow-parser-oxidized.
#
# Why the cp + rename + babel pattern (and not `babel src --out-dir dist
# --copy-files`):
# 1. We need `.js.flow` shadow files for every Flow-annotated `.js` so Flow
#    can still type-check downstream consumers. Babel does not emit those.
# 2. The in-place transform pattern matches upstream verbatim, simplifying any
#    future divergence audit.

set -xe -o pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_DIR="$(cd "$THIS_DIR/.." && pwd)"

# Packages with src/ that need building into dist/. Skipped here:
#   - flow-estree-oxidized (main: src/index.js — no build needed)
#   - prettier-plugin-flow-parser-oxidized (ships pre-built index.mjs)
PACKAGES=(
  flow-parser-oxidized
  flow-eslint-oxidized
  flow-transform-oxidized
  babel-plugin-syntax-flow-parser-oxidized
)

yarn install --offline

for package in "${PACKAGES[@]}"; do
  PACKAGE_DIR="$WORKSPACE_DIR/$package"
  DIST_DIR="$PACKAGE_DIR/dist"
  SRC_DIR="$PACKAGE_DIR/src"

  rm -rf "$DIST_DIR"
  cp -r "$SRC_DIR" "$DIST_DIR"

  # Create `.js.flow` shadow files for every `@flow`-annotated source file. Flow
  # has no system to emit declaration files from implementation files, so we
  # rename each Flow-annotated `.js` to `.js.flow` (which Flow treats as a
  # declaration). Downstream consumers importing from this package's dist see
  # the flow types via the shadow file.
  find "$DIST_DIR" -type f -name "*.js" | while read -r file; do
    if grep -q " @flow" "$file"; then
      new_file="${file}.flow"
      if [ ! -f "$new_file" ]; then
        cp "$file" "$new_file"
      fi
    fi
  done

  # Re-copy the raw .js files (the loop above turned some into .js.flow, but
  # we still need the runnable .js form). rsync preserves directory structure
  # and only copies .js files (no .js.flow, no .json).
  rsync -a --include="*/" --include="*.js" --exclude="*" "$SRC_DIR/" "$DIST_DIR/"

  # Run babel in-place over dist/. preset-env + flow-strip + class-properties +
  # flow-enums plugins are picked up from babel.config.js at the workspace root.
  yarn babel --config-file="$WORKSPACE_DIR/babel.config.js" "$DIST_DIR" --out-dir="$DIST_DIR"
done

# Build and embed the Flow Rust parser WASM into flow-parser-oxidized/dist/.
# Mirrors upstream hermes-parser/scripts/build.sh's `genWasmParser.js
# "$WASM_PARSER"` step. The internal facebook script invokes buck2 to compile
# the Rust parser to emscripten WASM and prints the path; genFlowWasmParser.js
# wraps that path's contents with the MIT license header and writes
# flow-parser-oxidized/dist/FlowParserWASM.js.
FB_BUILD_WASM_PARSER="$THIS_DIR/facebook/buildFlowWasmParser.sh"
if [[ -x "$FB_BUILD_WASM_PARSER" ]]; then
  WASM_PARSER=$("$FB_BUILD_WASM_PARSER")
  node "$THIS_DIR/genFlowWasmParser.js" "$WASM_PARSER"
fi
