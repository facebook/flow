#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

function run() {
  if [[ -n $1 ]]; then
      args+=("$1")
  fi
  queries_in_file "type-at-pos" "exports-class-cjs.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-interface.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-class-as-type.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-class_of_poly_instance-es6.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-class_of_poly_instance-cjs.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-default.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-rec-export.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-star.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-typeof-class.js" "${args[@]}"
  queries_in_file "type-at-pos" "module-export-0.js" "${args[@]}"
  queries_in_file "type-at-pos" "module-export-1.js" "${args[@]}"
  queries_in_file "type-at-pos" "module-import.js" "${args[@]}"
  queries_in_file "type-at-pos" "require-class.js" "${args[@]}"
  queries_in_file "type-at-pos" "import-type.js" "${args[@]}"
  queries_in_file "type-at-pos" "rtype-main.js" "${args[@]}"
}

# Run using typed AST for imports
run ""

# Run without using typed AST for imports
run "--do_not_use_typed_AST_for_imports"
