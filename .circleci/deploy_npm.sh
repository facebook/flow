#!/bin/bash -e

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

if [ -f ~/.npmrc.bak ]; then mv ~/.npmrc.bak ~/.npmrc; fi
