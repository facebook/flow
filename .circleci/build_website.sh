#!/bin/bash -x

PAGES_CHECKOUT="$CIRCLE_WORKING_DIRECTORY/dist/flow.org"
export BUNDLE_GEMFILE="$CIRCLE_WORKING_DIRECTORY/website/Gemfile"

GEN_DIR=$([[ "$CIRCLE_TAG" = "" ]] && echo "master" || echo "$CIRCLE_TAG")
mkdir -p "$PAGES_CHECKOUT"
mkdir -p "website/static/$GEN_DIR"
cp "bin/flow.js" "website/static/${GEN_DIR}/flow.js"
cp -r "lib" "website/static/${GEN_DIR}/flowlib"
(cd website && env \
  JEKYLL_ENV=production \
  make build DEST="$PAGES_CHECKOUT")
