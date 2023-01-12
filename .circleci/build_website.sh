#!/bin/bash -x
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

export BUNDLE_GEMFILE="$CIRCLE_WORKING_DIRECTORY/website/Gemfile"

GEN_DIR=$([[ "$CIRCLE_TAG" = "" ]] && echo "master" || echo "$CIRCLE_TAG")
mkdir -p "website/static/flow/$GEN_DIR"
cp "bin/flow.js" "website/static/flow/${GEN_DIR}/flow.js"
cp -r "lib" "website/static/flow/${GEN_DIR}/flowlib"
cd website && INCLUDE_PAST_RELEASES=1 yarn build
