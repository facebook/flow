#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Drives the parser fixture suite under flow/src/parser/test/flow against the
# wasm-built Flow parser. After the workspace consolidation, src/ files use ES
# module syntax (matching upstream xplat/static_h hermes-parser shape), so the
# JS driver must require the babel-transformed oxidized/ build. oxidized/ is populated
# by the workspace-root `yarn build`, which itself invokes the FB-only wasm
# build script and writes flow-parser/oxidized/FlowParserWASM.js.

set -e -o pipefail

FBCODE_DIR=${1:?"missing FBCODE_DIR arg"}
FIXTURE_REL=${2:?"missing FIXTURE_REL arg"}
# Resolve everything to absolute paths BEFORE any `cd`, since FBCODE_DIR may
# be `.` and we cd into the workspace root before opening the dist flock.
FBCODE_ABS=$(cd "$FBCODE_DIR" && pwd -P)

WORKSPACE_DIR="$FBCODE_ABS/flow/packages"
PKG_DIR="$WORKSPACE_DIR/flow-parser"

cd "$WORKSPACE_DIR"

# Serialize dist/ rebuilds + reads against runOxidizedJestTests.sh and
# runContractTests.sh, which also rebuild the oxidized parser output
# under `yarn build`. Buck schedules these targets in parallel; without this
# flock, target A's node test can read a torn `oxidized/FlowParser.js` while
# target B's `yarn build` is mid-flight. Hold the lock through both
# `yarn build` AND the node run so a concurrent target can't clobber `oxidized/`
# while we're reading from it.
DIST_FLOCK="$PKG_DIR/.dist.flock"
exec 9> "$DIST_FLOCK"
echo "==> waiting for exclusive lock on $DIST_FLOCK"
flock -x 9
echo "==> acquired lock on $DIST_FLOCK"

# yarn.lock is regenerated on first install. Subsequent runs reuse the locked
# tree. We rely on the global fbcode yarn offline mirror so --offline is the
# right mode here.
yarn install --offline

# Build output for each package (and embed the WASM parser into
# flow-parser/oxidized/FlowParserWASM.js). The fixture runner requires
# `../oxidized/FlowParser` directly so it can pass fixture-specific parser
# options without the public `oxidized/index.js` defaults.
yarn build

# Use the fbsource third-party Node toolchain (24.x). The system Node may be
# 16.x which lacks `os.availableParallelism()` — keep it consistent with the
# other oxidized test runners.
NODE="$FBCODE_ABS/../xplat/third-party/node/bin/node"

# Run the full fixture suite in a single node process. Earlier revisions
# sharded across one process per section directory because the wasm parser
# leaked ~10MB of heap per fixture and a single process OOMed at 8GB; the
# string-buffer redesign moved per-parse string storage into a side buffer
# that's freed with the parse result, so a single process now handles all
# ~1663 fixtures in <1s with ~130MB RSS.
FIXTURE_ROOT="$FBCODE_ABS/$FIXTURE_REL"
DRIVER="$PKG_DIR/__tests__/runWasmFixtures.js"

FLOW_PARSER_FIXTURE_ROOT="$FIXTURE_ROOT" \
    "$NODE" --max-old-space-size=8192 --test "$DRIVER"
