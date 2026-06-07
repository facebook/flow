#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

usage() {
  echo "Usage: $0 OUTPUT_PATH" >&2
}

if [[ $# -ne 1 ]]; then
  usage
  exit 2
fi

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLOW_DIR="$(realpath "$THIS_DIR/.."/..)"
REPO_DIR="$(realpath "$FLOW_DIR/../..")"
RUST_PORT_DIR="$FLOW_DIR/rust_port"
OUTPUT="$1"
PROFILE="${FLOW_PARSER_WASM_PROFILE:-release}"

case "$PROFILE" in
  release|opt)
    CARGO_PROFILE_ARGS=(
      --release
      "-Zbuild-std=std,panic_abort"
    )
    CARGO_TARGET_PROFILE=release
    ;;
  dev|debug)
    CARGO_PROFILE_ARGS=()
    CARGO_TARGET_PROFILE=debug
    ;;
  *)
    echo "Unknown FLOW_PARSER_WASM_PROFILE '$PROFILE'. Expected dev or release." >&2
    exit 2
    ;;
esac

INTERNAL_RUST_BIN="$REPO_DIR/xplat/rust/toolchain/current/basic/bin"
if [[ -d "$INTERNAL_RUST_BIN" ]]; then
  export PATH="$INTERNAL_RUST_BIN:$PATH"
fi

export RUSTC_BOOTSTRAP="${RUSTC_BOOTSTRAP:-1}"
export CARGO_TARGET_DIR="${CARGO_TARGET_DIR:-$RUST_PORT_DIR/target}"

CARGO_CONFIG="$REPO_DIR/third-party/rust/.cargo/config.toml"
CARGO_ARGS=(
  --manifest-path "$RUST_PORT_DIR/Cargo.toml"
  --package flow_parser_wasm
  --target wasm32-unknown-emscripten
  --lib
  --crate-type staticlib
)
if [[ -f "$CARGO_CONFIG" ]]; then
  CARGO_ARGS+=(--config "$CARGO_CONFIG" --offline --locked)
fi
if [[ "$PROFILE" == "release" || "$PROFILE" == "opt" ]]; then
  # Keep this in sync with Buck's internal opt wasm-emscripten Rust action.
  CARGO_ARGS+=(
    --config 'target.wasm32-unknown-emscripten.rustflags=["-Zunstable-options","-Crelocation-model=static","-Cpanic=abort","-Cpanic=immediate-abort","-Cembed-bitcode=no","-Ccodegen-units=1","-Cdebug-assertions=off","-Cdebuginfo=0","-Copt-level=z"]'
  )
fi

EMCC_BIN="${EMCC:-emcc}"
if ! command -v "$EMCC_BIN" >/dev/null 2>&1; then
  echo "emcc is required to build flow-parser-wasm.js. Install Emscripten or set EMCC." >&2
  exit 1
fi

"${CARGO:-cargo}" rustc "${CARGO_ARGS[@]}" "${CARGO_PROFILE_ARGS[@]}"

RUST_STATICLIB="$CARGO_TARGET_DIR/wasm32-unknown-emscripten/$CARGO_TARGET_PROFILE/libflow_parser_wasm.a"
if [[ ! -f "$RUST_STATICLIB" ]]; then
  echo "Expected Rust static library was not produced: $RUST_STATICLIB" >&2
  exit 1
fi

mkdir -p "$(dirname "$OUTPUT")"
rm -f "$OUTPUT"

"$EMCC_BIN" \
  "$RUST_STATICLIB" \
  -o "$OUTPUT" \
  -sALLOW_MEMORY_GROWTH=1 \
  -sWASM=1 \
  -sBINARYEN_ASYNC_COMPILATION=0 \
  -sENVIRONMENT=node \
  "-sEXPORTED_FUNCTIONS=['_malloc','_free','_main','_hermesParse','_hermesParseResult_free','_hermesParseResult_getError','_hermesParseResult_getErrorLine','_hermesParseResult_getErrorColumn','_hermesParseResult_getProgramBuffer','_hermesParseResult_getPositionBuffer','_hermesParseResult_getPositionBufferSize','_hermesParseResult_getStringBuffer']" \
  "-sEXPORTED_RUNTIME_METHODS=['cwrap','ccall']" \
  -sEXPORT_NAME=hermes_parser_wasm \
  -sINITIAL_MEMORY=128MB \
  -sMODULARIZE=1 \
  -sNODEJS_CATCH_EXIT=0 \
  -sNODEJS_CATCH_REJECTION=0 \
  -sNODERAWFS=0 \
  -sSINGLE_FILE=1 \
  -sSTACK_SIZE=64MB \
  -sWASM_ASYNC_COMPILATION=0 \
  -Os

echo "Built $OUTPUT"
