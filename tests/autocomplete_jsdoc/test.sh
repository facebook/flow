#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "jsdoc.js" --lsp
queries_in_file autocomplete "jsdoc-members-1.js" --lsp
queries_in_file autocomplete "jsdoc-members-2.js" --lsp
queries_in_file autocomplete "jsdoc-members-3.js" --lsp
queries_in_file autocomplete "jsdoc-members-4.js" --lsp
queries_in_file autocomplete "jsdoc-members-5.js" --lsp
queries_in_file autocomplete "jsdoc-members-6.js" --lsp
queries_in_file autocomplete "jsdoc-members-7.js" --lsp
queries_in_file autocomplete "jsdoc-members-8.js" --lsp
queries_in_file autocomplete "types-jsdoc.js" --lsp
queries_in_file autocomplete "qualified-types-jsdoc.js" --lsp
query_at_pos autocomplete "jsdoc_comment.js" 3 4 --lsp
query_at_pos autocomplete "jsdoc_comment.js" 8 4 --lsp
query_at_pos autocomplete "jsdoc_comment.js" 11 4 --lsp
