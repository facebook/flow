#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

query_at_pos autocomplete "keyword.js" 1 1 --pretty
queries_in_file autocomplete "records.js" --pretty --lsp
queries_in_file autocomplete "fields.js" --pretty --lsp
