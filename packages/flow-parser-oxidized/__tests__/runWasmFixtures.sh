#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

FBCODE_DIR=${1:?"missing FBCODE_DIR arg"}
FIXTURE_REL=${2:?"missing FIXTURE_REL arg"}

PKG_DIR="$FBCODE_DIR/flow/packages/flow-parser-oxidized"
PKG_SRC="$PKG_DIR/src"
WASM_OUT="$PKG_SRC/FlowParserWASM.js"

# Build the wasm under emcc. --isolation-dir is required because we are nested
# inside an outer buck2 test invocation with different state.
WASM_PATH=$(buck2 --isolation-dir flow_wasm_test build \
    @fbsource//xplat/mode/hermes/opt \
    @fbsource//xplat/mode/emcc \
    fbsource//xplat/hermes/tools/flow-parser-wasm:flow-parser-wasm.js \
    --show-full-output 2>&1 \
    | awk '/flow-parser-wasm\.js$/ {print $2; exit}')

if [ -z "$WASM_PATH" ] || [ ! -f "$WASM_PATH" ]; then
    echo "Failed to build flow-parser-wasm.js (got '$WASM_PATH')" >&2
    exit 1
fi

# PID-counted cleanup — `runContractTests.sh` and this script both write to the
# same `$WASM_OUT` (the JS test harness `require()`s a hard-coded
# `./FlowParserWASM` path so we can't trivially split the artifacts). When buck
# schedules both targets in parallel, a naive `trap rm -f` from one would
# delete the file mid-run for the other. Instead, each script registers its
# PID under a shared lock dir; on exit each removes its own PID and only the
# last one out (`rmdir` succeeds == dir is empty) deletes `$WASM_OUT`.
# Install the cleanup trap BEFORE writing $WASM_OUT so the file is removed
# even if the cat below fails or the script is interrupted.
WASM_LOCK_DIR="${WASM_OUT}.lock.d"
mkdir -p "$WASM_LOCK_DIR"
WASM_MARKER="$WASM_LOCK_DIR/$$"
mkdir "$WASM_MARKER"
trap '
    DRIVER_EXIT_CODE=$?
    rmdir "$WASM_MARKER" 2>/dev/null || true
    if rmdir "$WASM_LOCK_DIR" 2>/dev/null; then
        rm -f "$WASM_OUT" || true
    fi
    exit "$DRIVER_EXIT_CODE"
' EXIT

{
    # The license header below is the contents of the GENERATED
    # FlowParserWASM.js file, not metadata for this script. Built line-by-line
    # so license-lint doesn't treat it as a duplicate of the header at the top
    # of this file.
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

# NOTE on the build step: the fixture runner intentionally bypasses the
# public `flow-parser-oxidized` entry (which goes through `dist/index.js`
# after Phase C5 / #11) — the JS driver requires `../src/FlowParser` directly
# so the fixture suite sees the raw OCaml-shape AST that the `.tree.json`
# expectations were captured against. That's why we don't need to run
# `yarn install` / `yarn build` here. Contract tests (runContractTests.sh)
# do need a built `dist/` because they go through the public entry.
#
# Run the full fixture suite in a single node process. Earlier revisions
# sharded across one process per section directory because the wasm parser
# leaked ~10MB of heap per fixture and a single process OOMed at 8GB; the
# string-buffer redesign moved per-parse string storage into a side buffer
# that's freed with the parse result, so a single process now handles all
# ~1663 fixtures in <1s with ~130MB RSS.
FIXTURE_ROOT="$FBCODE_DIR/$FIXTURE_REL"
DRIVER="$FBCODE_DIR/flow/packages/flow-parser-oxidized/__tests__/runWasmFixtures.js"

FLOW_PARSER_FIXTURE_ROOT="$FIXTURE_ROOT" \
    node --max-old-space-size=8192 --test "$DRIVER"
