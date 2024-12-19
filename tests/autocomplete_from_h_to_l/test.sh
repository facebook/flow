#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "identifier_import_type.js" --pretty
queries_in_file autocomplete "identifier.js" --pretty
queries_in_file autocomplete "identifier_middle.js" --pretty
queries_in_file autocomplete "identifier_type.js" --pretty
queries_in_file autocomplete "if.js" --pretty
queries_in_file autocomplete "import_source.js" --pretty
queries_in_file autocomplete "import_specifiers_1.js" --pretty
queries_in_file autocomplete "import_specifiers_2.js" --pretty
queries_in_file autocomplete "indexed-access-1.js" --lsp
queries_in_file autocomplete "indexed-access-2.js" --lsp
queries_in_file autocomplete "indexed-access-3.js" --lsp
queries_in_file autocomplete "indexed-access-4.js" --lsp
queries_in_file autocomplete "indexed-access-optional-1.js" --lsp
queries_in_file autocomplete "indexed-access-optional-2.js" --lsp
queries_in_file autocomplete "indirect_array.js" --pretty
queries_in_file autocomplete "inherited-class-properties.js" --pretty
queries_in_file autocomplete "intersection.js" --pretty
queries_in_file autocomplete "issue-1368.js" --pretty
queries_in_file autocomplete "iterator.js" --lsp
queries_in_file autocomplete "keywords_after_export_default.js" --pretty
queries_in_file autocomplete "keywords_c.js" --pretty
queries_in_file autocomplete "keywords_f.js" --pretty
query_at_pos autocomplete "last_empty_line.js" 5 0 --pretty
queries_in_file autocomplete "literal.js" --pretty
