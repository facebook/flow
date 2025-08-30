#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "keyword-expression.js" --pretty
queries_in_file autocomplete "pattern-identifier.js" --pretty
queries_in_file autocomplete "pattern-binding.js" --pretty
queries_in_file autocomplete "pattern-object.js" --pretty
queries_in_file autocomplete "pattern-object-rest.js" --pretty
queries_in_file autocomplete "pattern-member.js" --pretty
