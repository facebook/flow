#!/bin/sh
FLOW=$1

ignore_declaration_files() {
  mv A.js.flow A.js.flow.ignored
  mv ExplicitProvidesModuleDifferentNameDefinitions.js.flow ExplicitProvidesModuleDifferentNameDefinitions.js.flow.ignored
  mv ExplicitProvidesModuleSameName.js.flow ExplicitProvidesModuleSameName.js.flow.ignored
  mv ImplicitProvidesModule.js.flow ImplicitProvidesModule.js.flow.ignored
  mv external/_d3/min.js.flow external/_d3/min.js.flow.ignored
  mv node_modules/qux/corge/lib/index.js.flow node_modules/qux/corge/lib/index.js.flow.ignored
  mv node_modules/qux/docblock.js.flow node_modules/qux/docblock.js.flow.ignored
}

use_declaration_files() {
  mv A.js.flow.ignored A.js.flow
  mv ExplicitProvidesModuleDifferentNameDefinitions.js.flow.ignored ExplicitProvidesModuleDifferentNameDefinitions.js.flow
  mv ExplicitProvidesModuleSameName.js.flow.ignored ExplicitProvidesModuleSameName.js.flow
  mv ImplicitProvidesModule.js.flow.ignored ImplicitProvidesModule.js.flow
  mv external/_d3/min.js.flow.ignored external/_d3/min.js.flow
  mv node_modules/qux/corge/lib/index.js.flow.ignored node_modules/qux/corge/lib/index.js.flow
  mv node_modules/qux/docblock.js.flow.ignored node_modules/qux/docblock.js.flow
}

ignore_implementation_files() {
  mv A.js tests/declaration_files_incremental_haste/A.js.ignored
  mv ExplicitProvidesModuleDifferentName.js tests/declaration_files_incremental_haste/ExplicitProvidesModuleDifferentName.js.ignored
  mv ExplicitProvidesModuleSameName.js tests/declaration_files_incremental_haste/ExplicitProvidesModuleSameName.js.ignored
  mv external/_d3/min.js tests/declaration_files_incremental_haste/external/_d3/min.js.ignored
  mv foo/bar/nested_test.js tests/declaration_files_incremental_haste/foo/bar/nested_test.js.ignored
  mv ImplicitProvidesModule.js tests/declaration_files_incremental_haste/ImplicitProvidesModule.js.ignored
  mv md5.js tests/declaration_files_incremental_haste/md5.js.ignored
  mv node_modules/qux/corge/lib/index.js tests/declaration_files_incremental_haste/node_modules/qux/corge/lib/index.js.ignored
  mv node_modules/qux/docblock.js tests/declaration_files_incremental_haste/node_modules/qux/docblock.js.ignored
  mv test.js tests/declaration_files_incremental_haste/test.js.ignored
  mv ws/index.js tests/declaration_files_incremental_haste/ws/index.js.ignored
  mv ws/test/client.js tests/declaration_files_incremental_haste/ws/test/client.js.ignored
}

use_implementation_files() {
  mv A.js.ignored tests/declaration_files_incremental_haste/A.js
  mv ExplicitProvidesModuleDifferentName.js.ignored tests/declaration_files_incremental_haste/ExplicitProvidesModuleDifferentName.js
  mv ExplicitProvidesModuleSameName.js.ignored tests/declaration_files_incremental_haste/ExplicitProvidesModuleSameName.js
  mv external/_d3/min.js.ignored tests/declaration_files_incremental_haste/external/_d3/min.js
  mv foo/bar/nested_test.js.ignored tests/declaration_files_incremental_haste/foo/bar/nested_test.js
  mv ImplicitProvidesModule.js.ignored tests/declaration_files_incremental_haste/ImplicitProvidesModule.js
  mv md5.js.ignored tests/declaration_files_incremental_haste/md5.js
  mv node_modules/qux/corge/lib/index.js.ignored tests/declaration_files_incremental_haste/node_modules/qux/corge/lib/index.js
  mv node_modules/qux/docblock.js.ignored tests/declaration_files_incremental_haste/node_modules/qux/docblock.js
  mv test.js.ignored tests/declaration_files_incremental_haste/test.js
  mv ws/index.js.ignored tests/declaration_files_incremental_haste/ws/index.js
  mv ws/test/client.js.ignored tests/declaration_files_incremental_haste/ws/test/client.js
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
