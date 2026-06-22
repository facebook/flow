#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -xe -o pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FLOW_PARSER_SRC_DIR="$THIS_DIR/../flow-parser/oxidized-src"

PACKAGES=(
  flow-estree
  flow-parser
  flow-eslint
  flow-transform
  babel-plugin-syntax-flow-parser
)

BOOTSTRAP_PACKAGES=(
  flow-estree
  flow-parser
  babel-plugin-syntax-flow-parser
)

# The parser override in babel.config.js requires the workspace plugin to be
# built. Disable it for the bootstrap pass below; we unset just before the final
# pass when the chain is ready.
export SKIP_HERMES_PARSER_OVERRIDE=1

# Yarn install all packages
yarn install --offline

# Use internal FB build or pass path to WASM parser as first command line argument
FB_BUILD_WASM_PARSER="$THIS_DIR/facebook/buildFlowWasmParser.sh"
if [[ -f "${1:-}" ]]; then
  WASM_PARSER="$1"
elif [[ -f "$FB_BUILD_WASM_PARSER" ]]; then
  WASM_PARSER=$("$FB_BUILD_WASM_PARSER")
else
  echo "Failed to get WASM parser" 1>&2
  exit 1
fi

package_src_dir() {
  local package="$1"
  if [[ "$package" == "flow-parser" ]]; then
    echo "$FLOW_PARSER_SRC_DIR"
  else
    echo "$THIS_DIR/../$package/src"
  fi
}

package_dist_dir() {
  local package="$1"
  echo "$THIS_DIR/../$package/dist"
}

# Create fresh dist directory for each package, and copy source files in
for package in "${PACKAGES[@]}"; do
  DIST_DIR="$(package_dist_dir "$package")"
  SRC_DIR="$(package_src_dir "$package")"

  if [[ ! -d "$SRC_DIR" ]]; then
    continue
  fi

  # Clean dist
  rm -rf "$DIST_DIR"
  cp -r "$SRC_DIR" "$DIST_DIR"

  # There is no system for flow to emit flow declarations for files
  # So we rename all the JS files to .js.flow so they are treated like flow declarations
  find "$DIST_DIR" -type f -name "*.js" | while read -r file; do
    # Check if file contains flow annotation
    if grep -q " @flow" "$file"; then
      # Create a new file with .js.flow extension
      new_file="${file}.flow"
      # Only proceed if the destination doesn't already exist
      if [ ! -f "$new_file" ]; then
        cp "$file" "$new_file"
      fi
    fi
  done

  # Copy just the JS files again
  if [[ "$package" == "flow-parser" ]]; then
    rsync -a --include="*/" --include="*.js" --exclude="*" "$SRC_DIR/" "$DIST_DIR/"
  else
    rsync -a --include="*/" --include="*.js" --exclude="*" "$SRC_DIR" "$DIST_DIR"
  fi
done

# Generate source code that only applies to dist directory
node "$THIS_DIR/genFlowWasmParser.js" "$WASM_PARSER"

# Bootstrap pass: strip Flow from the parser-plugin chain first, with the
# parser override still disabled (SKIP_HERMES_PARSER_OVERRIDE is set above).
# After this loop their dist/index.js files are valid plain JS, so the
# override can be re-enabled for the remaining packages.
for package in "${BOOTSTRAP_PACKAGES[@]}"; do
  PACKAGE_DIST_DIR="$(package_dist_dir "$package")"
  if [[ ! -d "$PACKAGE_DIST_DIR" ]]; then
    continue
  fi
  BABEL_IGNORE_ARGS=()
  if [[ "$package" == "flow-parser" ]]; then
    BABEL_IGNORE_ARGS=(
      --ignore
      "$PACKAGE_DIST_DIR/transform/print/**"
    )
  fi
  yarn babel \
    --config-file="$THIS_DIR/../babel.config.js" \
    "$PACKAGE_DIST_DIR" \
    --out-dir="$PACKAGE_DIST_DIR" \
    "${BABEL_IGNORE_ARGS[@]}"
done

# Re-enable the override so the remaining packages can be parsed with
# flow-parser (they contain Flow `as` cast syntax).
unset SKIP_HERMES_PARSER_OVERRIDE

for package in "${PACKAGES[@]}"; do
  # Skip packages already processed in the bootstrap pass above.
  case " ${BOOTSTRAP_PACKAGES[*]} " in
    *" $package "*)
      if [[ "$package" != "flow-parser" ]]; then
        continue
      fi
      ;;
  esac
  PACKAGE_DIST_DIR="$(package_dist_dir "$package")"
  if [[ ! -d "$PACKAGE_DIST_DIR" ]]; then
    continue
  fi
  yarn babel --config-file="$THIS_DIR/../babel.config.js" "$PACKAGE_DIST_DIR" --out-dir="$PACKAGE_DIST_DIR"
done

# Validate that the generated flow files are sane
# We don't bother validating the raw-js files as they are validated by babel first
yarn eslint \
  "*/dist/**/*.js.flow" \
  --no-ignore
