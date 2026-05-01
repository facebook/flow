#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Single-package build for flow-parser-oxidized. Mirrors upstream
# hermes-parser/scripts/build.sh but scoped to just this package — it does the
# `cp src dist` then `babel dist --out-dir dist` in-place transform pattern,
# producing a CJS-shape `dist/` from the ESM-shape `src/`. Also creates `.flow`
# shadow files for any `.js` containing an `@flow` annotation, so downstream
# Flow consumers can still see the type information.
#
# Why this pattern (and not `babel src --out-dir dist --copy-files`):
# 1. We need `.js.flow` shadow files for every Flow-annotated `.js` so Flow
#    can still type-check downstream consumers.
# 2. `babel --copy-files` would also copy `.js.flow` files unmodified into
#    `dist/`, which is what we want.
# 3. The in-place pattern matches upstream verbatim, which simplifies any
#    future divergence audit.

set -xe -o pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(cd "$THIS_DIR/.." && pwd)"
DIST_DIR="$PACKAGE_DIR/dist"
SRC_DIR="$PACKAGE_DIR/src"

# Clean dist and copy src into it. We do an in-place transform below (babel
# overwrites each .js); the rsync step rescues any non-JS files (e.g. .json,
# the wasm wrapper, etc.) from the original src/ tree.
rm -rf "$DIST_DIR"
cp -r "$SRC_DIR" "$DIST_DIR"

# Create `.js.flow` shadow files for every `@flow`-annotated source file. This
# matches upstream — Flow has no system to emit declaration files from
# implementation files, so we just rename each Flow-annotated `.js` to
# `.js.flow` (which Flow treats as a declaration). Downstream consumers
# importing from this package's dist see the flow types via the shadow file.
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
# and only copies .js files (no .js.flow, no .json). This step is
# idempotent — files already at the destination are simply overwritten.
rsync -a --include="*/" --include="*.js" --exclude="*" "$SRC_DIR/" "$DIST_DIR/"

# Run babel in-place over dist/. preset-env + the flow-strip + class-properties
# + flow-enums plugins are picked up from babel.config.js at the package root.
yarn babel --config-file="$PACKAGE_DIR/babel.config.js" "$DIST_DIR" --out-dir="$DIST_DIR"

# Build and embed the Flow Rust parser WASM. Mirrors upstream
# hermes-parser/scripts/build.sh's `genWasmParser.js "$WASM_PARSER"` step.
# The internal facebook script invokes buck2 to compile the Rust parser to
# emscripten WASM and prints the path; genFlowWasmParser.js wraps that path's
# contents with the MIT license header and writes dist/FlowParserWASM.js.
FB_BUILD_WASM_PARSER="$THIS_DIR/facebook/buildFlowWasmParser.sh"
if [[ -x "$FB_BUILD_WASM_PARSER" ]]; then
  WASM_PARSER=$("$FB_BUILD_WASM_PARSER")
  node "$THIS_DIR/genFlowWasmParser.js" "$WASM_PARSER"
fi
