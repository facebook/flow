#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WEBSITE=$(realpath "$DIR/../website")
export BUNDLE_GEMFILE="$WEBSITE/Gemfile"
source "$HOME/.rvm/scripts/rvm"

# We need flow.js to exist, so let's build it
(cd "$DIR/../" && make js)

FLOW_OUT_DIR=$(realpath "$DIR/../bin")

rm -rf "_site/"
rm -rf "$WEBSITE/.asset-cache"

# Symlink flow.js and the flowlibs into place directly in the _site folder since
# they are not managed by jekyll-assets.
mkdir -p "_site/static/master"
ln -sf "$FLOW_OUT_DIR/flow.js" "_site/static/master/flow.js"
ln -sf "$DIR/../lib" "_site/static/master/flowlib"

# PATH="$FLOW_OUT_DIR/flow:$PATH" bundle exec jekyll build --source "$WEBSITE" "$@"
PATH="$FLOW_OUT_DIR/flow:$PATH" bundle exec jekyll serve --source "$WEBSITE" --host :: --port 8080 "$@"
