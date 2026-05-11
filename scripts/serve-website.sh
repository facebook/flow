#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WEBSITE=$(realpath "$DIR/../website")
FLOW_WEBSITE_FLOW_JS_IMPL="${FLOW_WEBSITE_FLOW_JS_IMPL:-js-of-ocaml}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --flow-js=*)
      FLOW_WEBSITE_FLOW_JS_IMPL="${1#--flow-js=}"
      shift
      ;;
    --flow-js)
      FLOW_WEBSITE_FLOW_JS_IMPL="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

# We need flow.js to exist, so let's build it
case "$FLOW_WEBSITE_FLOW_JS_IMPL" in
  js-of-ocaml)
    (cd "$DIR/../" && make deps && make && make deps-js && make js)
    ;;
  rust-wasm)
    (cd "$DIR/../" && make deps && make && make js FLOW_JS_IMPL=rust-wasm)
    ;;
  *)
    echo "Unknown FLOW_WEBSITE_FLOW_JS_IMPL '$FLOW_WEBSITE_FLOW_JS_IMPL'" >&2
    exit 2
    ;;
esac

FLOW_OUT_DIR=$(realpath "$DIR/../bin")

pushd "$WEBSITE" > /dev/null

# Symlink flow.js and the flowlibs into place directly in the static folder
ln -sf "$FLOW_OUT_DIR/flow.js" "static/flow/master/flow.js"
ln -sfn "$DIR/../lib" "static/flow/master/flowlib"

yarn install
PATH="$FLOW_OUT_DIR:$PATH" yarn start --host ::
