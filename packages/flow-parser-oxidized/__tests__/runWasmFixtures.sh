#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

FBCODE_DIR=${1:?"missing FBCODE_DIR arg"}
FIXTURE_REL=${2:?"missing FIXTURE_REL arg"}

PKG_SRC="$FBCODE_DIR/flow/packages/flow-parser-oxidized/src"
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

# Install the cleanup trap BEFORE writing $WASM_OUT so the file is removed
# even if the cat below fails or the script is interrupted.
trap 'rm -f "$WASM_OUT"' EXIT

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
