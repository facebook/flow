#!/bin/bash -x
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

export BUNDLE_GEMFILE="$CIRCLE_WORKING_DIRECTORY/new_website/Gemfile"

mkdir -p ~/.ssh
ssh-keyscan github.com >> ~/.ssh/known_hosts
GEN_DIR=$([[ "$CIRCLE_TAG" = "" ]] && echo "master" || echo "$CIRCLE_TAG")
mkdir -p "new_website/static/flow/$GEN_DIR"
cp "bin/flow.js" "new_website/static/flow/${GEN_DIR}/flow.js"
cp -r "lib" "new_website/static/flow/${GEN_DIR}/flowlib"
cd new_website && yarn build
