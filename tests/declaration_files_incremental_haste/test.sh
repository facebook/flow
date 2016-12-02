#!/bin/sh
FLOW=$1

OVERLAPPING_FILES="
  A.js
  ExplicitProvidesModuleDifferentNameDefinitions.js
  ExplicitProvidesModuleSameName.js
  ImplicitProvidesModule.js
  external/_d3/min.js
  node_modules/qux/corge/lib/index.js
  node_modules/qux/docblock.js
"
DECL_FILES=$(echo "$OVERLAPPING_FILES" | sed -e "s/\.js/.js.flow/g")
IMPL_FILES="$OVERLAPPING_FILES md5.js ws/index.js ws/test/client.js"

ignore_files() {
  for file in $1; do
    mv "$file" "$file.ignored"
  done
}

use_files() {
  for file in $1; do
    mv "$file.ignored" "$file"
  done
}

printf "======Start off with the .js files but without the .flow file======\n"
"$FLOW" status .
use_files "$DECL_FILES"
"$FLOW" force-recheck $DECL_FILES
"$FLOW" status .
ignore_files "$DECL_FILES"
"$FLOW" force-recheck $DECL_FILES
"$FLOW" status .

printf "\n\n======Start off with the .js files and the .flow file======\n"
"$FLOW" stop .
use_files "$DECL_FILES"
"$FLOW" start . --all --wait

"$FLOW" status .
ignore_files "$DECL_FILES"
"$FLOW" force-recheck $DECL_FILES
"$FLOW" status .
use_files "$DECL_FILES"
"$FLOW" force-recheck $DECL_FILES
"$FLOW" status .

printf "\n\n======Start off without the .js files and with the .flow file======\n"
"$FLOW" stop .
ignore_files "$IMPL_FILES"
"$FLOW" start . --all --wait

"$FLOW" status .
use_files "$IMPL_FILES"
"$FLOW" force-recheck $IMPL_FILES
"$FLOW" status .
ignore_files "$IMPL_FILES"
"$FLOW" force-recheck $IMPL_FILES
"$FLOW" status .

# reset
use_files "$IMPL_FILES"
ignore_files "$DECL_FILES"
