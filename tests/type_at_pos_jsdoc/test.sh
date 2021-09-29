#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "type-at-pos" "variable_declaration.js" --strip-root --pretty
queries_in_file "type-at-pos" "class_declaration.js" --strip-root --pretty
queries_in_file "type-at-pos" "function_declaration.js" --strip-root --pretty
queries_in_file "type-at-pos" "declare.js" --strip-root --pretty
queries_in_file "type-at-pos" "objects-requires.js" --strip-root --pretty
queries_in_file "type-at-pos" "objects.js" --strip-root --pretty
queries_in_file "type-at-pos" "requires.js" --strip-root --pretty
queries_in_file "type-at-pos" "imports.js" --strip-root --pretty
queries_in_file "type-at-pos" "unrecognized_tags.js" --strip-root --pretty
queries_in_file "type-at-pos" "parse_error.js" --strip-root --pretty
queries_in_file "type-at-pos" "types.js" --strip-root --pretty
queries_in_file "type-at-pos" "redeclare_import.js" --strip-root --pretty
