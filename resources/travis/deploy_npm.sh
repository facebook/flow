#!/bin/bash -e

# make sure -x (debugging) is off so we don't print the token in the logs
set +x

# only run on tags
if [[ "$TRAVIS_TAG" = "" ]]; then exit 0; fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# shellcheck source=fbcode/flow/resources/travis/setup_opam.sh
source "$DIR/setup_opam.sh"
# shellcheck source=fbcode/flow/resources/travis/setup_node.sh
source "$DIR/setup_node.sh"

pushd packages/flow-parser > /dev/null

NPM_V=$(sed -n 's/.*"version":.*\([0-9]\{1,\}\.[0-9]\{1,\}\.[0-9]\{1,\}\).*/\1/p' package.json)
TAG_V=$(echo "${TRAVIS_TAG}" | sed -n 's/v\{0,\}\([0-9]\{1,\}\.[0-9]\{1,\}\.[0-9]\{1,\}\)/\1/p')
if [[ "$TAG_V" == "$NPM_V" ]]; then
  echo "Publishing flow-parser@${TAG_V}";
  if [ -f ~/.npmrc ]; then mv ~/.npmrc ~/.npmrc.bak; fi
  echo "//registry.npmjs.org/:_authToken=${NPM_TOKEN}" > ~/.npmrc
  npm publish
  if [ -f ~/.npmrc.bak ]; then mv ~/.npmrc.bak ~/.npmrc; fi
  echo "Success"
else
  echo "Publishing flow-parser skipped (versions not in alignment)"
fi

popd > /dev/null
