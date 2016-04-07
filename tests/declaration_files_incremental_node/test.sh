#!/bin/sh
FLOW=$1

ignore_declaration_files() {
  mv A.js.flow A.js.flow.ignored
  mv node_modules/B.js.flow node_modules/B.js.flow.ignored
  mv node_modules/package_with_dir_main/dir/index.js.flow node_modules/package_with_dir_main/dir/index.js.flow.ignored
  mv node_modules/package_with_full_main/code.js.flow node_modules/package_with_full_main/code.js.flow.ignored
  mv node_modules/package_with_no_package_json/index.js.flow node_modules/package_with_no_package_json/index.js.flow.ignored
  mv node_modules/package_with_partial_main/code.js.flow node_modules/package_with_partial_main/code.js.flow.ignored
  mv node_modules/package_with_dir_flow_main/dir/index.js.flow node_modules/package_with_dir_flow_main/dir/index.js.flow.ignored
  mv node_modules/package_with_full_flow_main/code.js.flow node_modules/package_with_full_flow_main/code.js.flow.ignored
  mv node_modules/package_with_partial_flow_main/code.js.flow node_modules/package_with_partial_flow_main/code.js.flow.ignored
}

use_declaration_files() {
  mv A.js.flow.ignored A.js.flow
  mv node_modules/B.js.flow.ignored node_modules/B.js.flow
  mv node_modules/package_with_dir_main/dir/index.js.flow.ignored node_modules/package_with_dir_main/dir/index.js.flow
  mv node_modules/package_with_full_main/code.js.flow.ignored node_modules/package_with_full_main/code.js.flow
  mv node_modules/package_with_no_package_json/index.js.flow.ignored node_modules/package_with_no_package_json/index.js.flow
  mv node_modules/package_with_partial_main/code.js.flow.ignored node_modules/package_with_partial_main/code.js.flow
  mv node_modules/package_with_dir_flow_main/dir/index.js.flow.ignored node_modules/package_with_dir_flow_main/dir/index.js.flow
  mv node_modules/package_with_full_flow_main/code.js.flow.ignored node_modules/package_with_full_flow_main/code.js.flow
  mv node_modules/package_with_partial_flow_main/code.js.flow.ignored node_modules/package_with_partial_flow_main/code.js.flow
}

ignore_implementation_files() {
  mv A.js A.js.ignored
  mv node_modules/B.js node_modules/B.js.ignored
  mv node_modules/package_with_dir_main/dir/index.js node_modules/package_with_dir_main/dir/index.js.ignored
  mv node_modules/package_with_full_main/code.js node_modules/package_with_full_main/code.js.ignored
  mv node_modules/package_with_no_package_json/index.js node_modules/package_with_no_package_json/index.js.ignored
  mv node_modules/package_with_partial_main/code.js node_modules/package_with_partial_main/code.js.ignored
  mv node_modules/package_with_dir_flow_main/dir/index.js node_modules/package_with_dir_flow_main/dir/index.js.ignored
  mv node_modules/package_with_full_flow_main/code.js node_modules/package_with_full_flow_main/code.js.ignored
  mv node_modules/package_with_partial_flow_main/code.js node_modules/package_with_partial_flow_main/code.js.ignored
}

use_implementation_files() {
  mv A.js.ignored A.js
  mv node_modules/B.js.ignored node_modules/B.js
  mv node_modules/package_with_dir_main/dir/index.js.ignored node_modules/package_with_dir_main/dir/index.js
  mv node_modules/package_with_full_main/code.js.ignored node_modules/package_with_full_main/code.js
  mv node_modules/package_with_no_package_json/index.js.ignored node_modules/package_with_no_package_json/index.js
  mv node_modules/package_with_partial_main/code.js.ignored node_modules/package_with_partial_main/code.js
  mv node_modules/package_with_dir_flow_main/dir/index.js.ignored node_modules/package_with_dir_flow_main/dir/index.js
  mv node_modules/package_with_full_main/code.js.ignored node_modules/package_with_full_flow_main/code.js
  mv node_modules/package_with_partial_flow_main/code.js.ignored node_modules/package_with_partial_flow_main/code.js
}

printf "======Start off with the .js files but without the .flow file======\n"
"$FLOW" status --old-output-format .
use_declaration_files
"$FLOW" status --old-output-format .
ignore_declaration_files
"$FLOW" status --old-output-format .

printf "\n\n======Start off with the .js files and the .flow file======\n"
"$FLOW" stop .
use_declaration_files
"$FLOW" start . --all --wait

"$FLOW" status --old-output-format .
ignore_declaration_files
"$FLOW" status --old-output-format .
use_declaration_files
"$FLOW" status --old-output-format .

printf "\n\n======Start off without the .js files and with the .flow file======\n"
"$FLOW" stop .
ignore_implementation_files
"$FLOW" start . --all --wait

"$FLOW" status --old-output-format .
use_implementation_files
"$FLOW" status --old-output-format .
ignore_implementation_files
"$FLOW" status --old-output-format .

# reset
use_implementation_files
ignore_declaration_files
