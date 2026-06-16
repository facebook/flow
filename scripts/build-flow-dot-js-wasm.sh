#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

usage() {
  echo "Usage: $0 [--output PATH] [--profile dev|release]" >&2
  echo "Defaults to --profile release. Use --profile dev for a faster, larger local build." >&2
}

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLOW_DIR="$(realpath "$DIR/..")"
REPO_DIR="$(realpath "$FLOW_DIR/../..")"
RUST_PORT_DIR="$FLOW_DIR/rust_port"
OUTPUT="$FLOW_DIR/bin/flow.js"
PROFILE="${FLOW_DOT_JS_WASM_PROFILE:-}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --output)
      OUTPUT="$2"
      shift 2
      ;;
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

is_truthy() {
  case "${1:-}" in
    ""|0|false|False|FALSE)
      return 1
      ;;
    *)
      return 0
      ;;
  esac
}

if [[ -z "$PROFILE" ]]; then
  if is_truthy "${FLOW_RELEASE:-${CI:-1}}"; then
    PROFILE=release
  else
    PROFILE=dev
  fi
fi

CARGO_PROFILE_ARGS=()
CARGO_TARGET_PROFILE=debug
case "$PROFILE" in
  release|opt)
    CARGO_PROFILE_ARGS=(
      --release
      "-Zbuild-std=std,panic_abort"
    )
    CARGO_TARGET_PROFILE=release
    ;;
  dev|debug)
    ;;
  *)
    echo "Unknown profile '$PROFILE'. Expected dev or release." >&2
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
  --package flow_dot_js_wasm
  --target wasm32-unknown-emscripten
  --lib
  --crate-type staticlib
)
if [[ -f "$CARGO_CONFIG" ]]; then
  CARGO_ARGS+=(--config "$CARGO_CONFIG" --locked --offline)
fi
if [[ "$PROFILE" == "release" || "$PROFILE" == "opt" ]]; then
  # Keep this in sync with Buck's target-platform wasm-emscripten Rust action.
  # The release build also rebuilds std above so these flags apply to the
  # sysroot, matching Buck's optimized wasm sysroot instead of rustup's prebuilt
  # std.
  CARGO_ARGS+=(
    --config 'target.wasm32-unknown-emscripten.rustflags=["-Zunstable-options","-Crelocation-model=static","-Cpanic=abort","-Cpanic=immediate-abort","-Cembed-bitcode=no","-Cdebug-assertions=on","-Copt-level=0","-Copt-level=z"]'
  )
fi

EMCC_BIN="${EMCC:-emcc}"
if ! command -v "$EMCC_BIN" >/dev/null 2>&1; then
  echo "emcc is required to build Rust flow.js. Install Emscripten or set EMCC." >&2
  exit 1
fi

"${CARGO:-cargo}" rustc "${CARGO_ARGS[@]}" "${CARGO_PROFILE_ARGS[@]}"

RUST_STATICLIB="$CARGO_TARGET_DIR/wasm32-unknown-emscripten/$CARGO_TARGET_PROFILE/libflow_dot_js_wasm.a"
if [[ ! -f "$RUST_STATICLIB" ]]; then
  echo "Expected Rust static library was not produced: $RUST_STATICLIB" >&2
  exit 1
fi

RAW_JS_DIR="$CARGO_TARGET_DIR/flow-dot-js-wasm/$CARGO_TARGET_PROFILE"
RAW_JS="$RAW_JS_DIR/flow_dot_js_wasm.js"
mkdir -p "$RAW_JS_DIR"
rm -f "$RAW_JS" "$RAW_JS_DIR/flow_dot_js_wasm.wasm"

"$EMCC_BIN" \
  "$RUST_STATICLIB" \
  -o "$RAW_JS"

mkdir -p "$(dirname "$OUTPUT")"
"${NODE:-node}" \
  "$FLOW_DIR/src/flow_dot_js_wasm_packager.js" \
  "$RAW_JS" \
  "$OUTPUT" \
  "$FLOW_DIR/src/flow_dot_js_wasm.js"

rm -f "$RAW_JS_DIR/flow_dot_js_wasm.wasm"

echo "Built $OUTPUT"
