#!/bin/bash -x
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

mkdir -p "website/static/flow/master"
mkdir -p "website/static/flow/master-rust-port"
cp "dist/flow.js" "website/static/flow/master/flow.js"
cp "dist-rust-port/flow.js" "website/static/flow/master-rust-port/flow.js"
rm -rf "website/static/flow/master/flowlib"
rm -rf "website/static/flow/master-rust-port/flowlib"
cp -r "lib" "website/static/flow/master/flowlib"
cp -r "lib" "website/static/flow/master-rust-port/flowlib"
cd website && INCLUDE_PAST_RELEASES=1 FLOW_WEBSITE_INCLUDE_RUST_PORT=1 FLOW_MAX_WORKERS=2 DOCUSAURUS_SSR_CONCURRENCY=2 yarn build
