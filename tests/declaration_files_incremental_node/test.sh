#!/bin/sh
FLOW=$1

IMPL_FILES="
  A.js
  node_modules/B.js
  node_modules/package_with_dir_main/dir/index.js
  node_modules/package_with_full_main/code.js
  node_modules/package_with_no_package_json/index.js
  node_modules/package_with_partial_main/code.js
"
DECL_FILES=$(echo "$IMPL_FILES" | sed -e "s/\.js/.js.flow/g")

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
