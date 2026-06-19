#!/bin/bash -x
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e -o pipefail

mkdir -p "website/static/flow/master"
cp "dist/flow.js" "website/static/flow/master/flow.js"
rm -rf "website/static/flow/master/flowlib"
cp -r "lib" "website/static/flow/master/flowlib"
cd website && INCLUDE_PAST_RELEASES=1 IN_FLOW_TEST=1 FLOW_MAX_WORKERS=2 DOCUSAURUS_SSR_CONCURRENCY=2 yarn build
