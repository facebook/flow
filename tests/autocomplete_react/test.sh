#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

queries_in_file autocomplete "jsx1.js" --pretty
queries_in_file autocomplete "jsx2.js" --pretty
queries_in_file autocomplete "jsx3.js" --pretty
queries_in_file autocomplete "jsx4.js" --pretty
queries_in_file autocomplete "jsx-attribute-member.js" --pretty
queries_in_file autocomplete "jsx-closing-tag.js" --pretty
queries_in_file autocomplete "jsx-closing-tag-unclosed.js" --pretty
queries_in_file autocomplete "jsx-function-component.js" --pretty
queries_in_file autocomplete "jsx-function-component-2.js" --pretty
queries_in_file autocomplete "jsx-function-component-3.js" --pretty
queries_in_file autocomplete "jsx-abstract-component.js" --pretty
queries_in_file autocomplete "jsx-with-children.js" --pretty
queries_in_file autocomplete "jsx-text.js" --pretty
queries_in_file autocomplete "components.js" --pretty
