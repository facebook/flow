#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Drives the hermes-parser contract test suite (ported under
# flow/packages/flow-parser-oxidized/__tests__/) against the live wasm-built
# Flow parser. Same wasm-bootstrap dance as runWasmFixtures.sh: build the wasm
# under emcc with --isolation-dir, drop it into src/FlowParserWASM.js, then
# invoke jest. Phase C7 (#14) populates the actual jest test files; this
# wrapper provides the wasm-aware run harness around them.

set -e -o pipefail

FBCODE_DIR=${1:?"missing FBCODE_DIR arg"}

PKG_DIR="$FBCODE_DIR/flow/packages/flow-parser-oxidized"
PKG_SRC="$PKG_DIR/src"
WASM_OUT="$PKG_SRC/FlowParserWASM.js"

WASM_PATH=$(buck2 --isolation-dir flow_contract_test build \
    @fbsource//xplat/mode/hermes/opt \
    @fbsource//xplat/mode/emcc \
    fbsource//xplat/hermes/tools/flow-parser-wasm:flow-parser-wasm.js \
    --show-full-output 2>&1 \
    | awk '/flow-parser-wasm\.js$/ {print $2; exit}')

if [ -z "$WASM_PATH" ] || [ ! -f "$WASM_PATH" ]; then
    echo "Failed to build flow-parser-wasm.js (got '$WASM_PATH')" >&2
    exit 1
fi

# PID-counted cleanup — `runWasmFixtures.sh` and this script both write to the
# same `$WASM_OUT` (the JS test harness `require()`s a hard-coded
# `./FlowParserWASM` path so we can't trivially split the artifacts). When buck
# schedules both targets in parallel, a naive `trap rm -f` from one would
# delete the file mid-run for the other. Instead, each script registers its
# PID under a shared lock dir; on exit each removes its own PID and only the
# last one out (`rmdir` succeeds == dir is empty) deletes `$WASM_OUT`.
WASM_LOCK_DIR="${WASM_OUT}.lock.d"
mkdir -p "$WASM_LOCK_DIR"
WASM_MARKER="$WASM_LOCK_DIR/$$"
mkdir "$WASM_MARKER"
trap '
    JEST_EXIT_CODE=$?
    rmdir "$WASM_MARKER" 2>/dev/null || true
    if rmdir "$WASM_LOCK_DIR" 2>/dev/null; then
        rm -f "$WASM_OUT" || true
    fi
    exit "$JEST_EXIT_CODE"
' EXIT

{
    # Same MIT header preamble as runWasmFixtures.sh — needed because the
    # wasm artifact ships without one and license-lint expects every JS file
    # in src/ to have it. Built echo-by-echo so license-lint doesn't false-
    # positive on a duplicate of this file's own header.
    echo '/**'
    echo ' * Copyright (c) Meta Platforms, Inc. and affiliates.'
    echo ' *'
    echo ' * This source code is licensed under the MIT license found in the'
    echo ' * LICENSE file in the root directory of this source tree.'
    echo ' */'
    echo ''
    echo "'use strict';"
    cat "$WASM_PATH"
} > "$WASM_OUT"

cd "$PKG_DIR"
# yarn.lock is regenerated on first install (devDependencies were added in
# Phase C6). Subsequent runs reuse the locked tree. We rely on the global
# fbcode yarn offline mirror so --offline is the right mode here.
yarn install --offline

# Build dist/ from src/ (Phase C5 / #11): contract tests `require()` the
# public entry `flow-parser-oxidized` which package.json `"main"` points at
# `dist/index.js`. Without this build, jest sees the ESM `src/` files
# directly and node can't `require()` them. babel-jest in the contract suite
# transforms test files on the fly, but the production import path goes
# through the built `dist/`.
yarn build

node --experimental-vm-modules ./node_modules/.bin/jest --no-coverage
