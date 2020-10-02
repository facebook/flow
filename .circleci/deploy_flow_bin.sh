#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set +x # don't print the secrets!

# only run on tags
if [[ "$CIRCLE_TAG" = "" ]]; then exit 0; fi

FLOW_BOT_NAME="flow-bot"
VERSION="${CIRCLE_TAG#v}"

BUILD_DIR=$(mktemp -d -t flow-bin-XXXXXXXXXX)
trap 'rm -rf "$BUILD_DIR"' EXIT

pushd "$BUILD_DIR"
git clone "https://${FLOW_BOT_NAME}:${FLOW_BOT_TOKEN}@github.com/flowtype/flow-bin.git"

pushd flow-bin

git config user.name "$FLOW_BOT_NAME"
git config user.email "$FLOW_BOT_EMAIL"

make push publish VERSION="$VERSION" NPM_TOKEN="$NPM_TOKEN"

popd > /dev/null

popd > /dev/null
