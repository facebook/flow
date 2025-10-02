#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "bar.js" --pretty
queries_in_file autocomplete "bool.js" --pretty
queries_in_file autocomplete "class_declaration_name.js" --pretty
queries_in_file autocomplete "class_extends.js" --pretty
queries_in_file autocomplete "class_members.js" --pretty
queries_in_file autocomplete "comments.js" --pretty
queries_in_file autocomplete "dispose.js" --pretty
queries_in_file autocomplete "enum-id.js" --pretty
queries_in_file autocomplete "enum-key.js" --lsp
queries_in_file autocomplete "enums.js" --lsp
queries_in_file autocomplete "eval_destructor.js" --pretty
queries_in_file autocomplete "exact.js" --pretty
query_at_pos autocomplete "fixme.js" 14 8 --lsp
query_at_pos autocomplete "fixme.js" 18 12 --pretty
query_at_pos autocomplete "fixme.js" 21 10 --lsp
query_at_pos autocomplete "fixme.js" 5 4 --lsp
query_at_pos autocomplete "fixme.js" 8 5 --lsp
queries_in_file autocomplete "foo.js" --pretty
queries_in_file autocomplete "foo_parse_fail.js" --pretty
queries_in_file autocomplete "function-added-properties.js" --pretty
queries_in_file autocomplete "function_builtins.js" --pretty
queries_in_file autocomplete "fun.js" --pretty
queries_in_file autocomplete "generic_alias.js" --pretty
queries_in_file autocomplete "generics.js" --pretty
