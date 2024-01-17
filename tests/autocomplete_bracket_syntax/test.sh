#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "bracket_syntax_1.js" --lsp
queries_in_file autocomplete "bracket_syntax_2.js" --lsp
queries_in_file autocomplete "bracket_syntax_3.js" --lsp
queries_in_file autocomplete "bracket_syntax_4.js" --lsp
queries_in_file autocomplete "bracket_syntax_5.js" --lsp
queries_in_file autocomplete "bracket_syntax_6.js" --lsp
queries_in_file autocomplete "bracket_syntax_needed.js" --lsp
queries_in_file autocomplete "bracket_syntax_needed_2.js" --lsp
