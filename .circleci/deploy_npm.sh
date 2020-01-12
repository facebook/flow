#!/bin/bash -e
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# make sure -x (debugging) is off so we don't print the token in the logs
set +x

# only run on tags
if [[ "$CIRCLE_TAG" = "" ]]; then exit 0; fi

if [ -f ~/.npmrc ]; then mv ~/.npmrc ~/.npmrc.bak; fi
echo "//registry.npmjs.org/:_authToken=${NPM_TOKEN}" > ~/.npmrc

echo "Publishing flow-parser-bin";
npm publish ./dist/npm-flow-parser-bin.tgz;

echo "Publishing flow-parser";
npm publish ./dist/npm-flow-parser.tgz;

echo "Publishing flow-remove-types";
npm publish ./dist/npm-flow-remove-types.tgz;

echo "Publishing flow-node";
npm publish ./dist/npm-flow-node.tgz;

if [ -f ~/.npmrc.bak ]; then mv ~/.npmrc.bak ~/.npmrc; fi
