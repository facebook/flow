#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WEBSITE=$(realpath "$DIR/../website")

# We need flow.js to exist, so let's build it
(cd "$DIR/../" && make deps && make && make deps-js && make js)

FLOW_OUT_DIR=$(realpath "$DIR/../bin")

pushd "$WEBSITE" > /dev/null

# Symlink flow.js and the flowlibs into place directly in the static folder
ln -sf "$FLOW_OUT_DIR/flow.js" "static/flow/master/flow.js"
ln -sfn "$DIR/../lib" "static/flow/master/flowlib"

yarn install
PATH="$FLOW_OUT_DIR:$PATH" yarn start --host ::
