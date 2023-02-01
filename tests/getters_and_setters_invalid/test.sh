#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

# Classes
assert_errors "$FLOW" check-contents setter_class_missing_param.js < setter_class_missing_param.js
queries_in_file type-at-pos setter_class_missing_param.js --pretty
assert_errors "$FLOW" check-contents setter_class_multiple_params.js < setter_class_multiple_params.js
queries_in_file type-at-pos setter_class_multiple_params.js --pretty

# Interfaces
assert_errors "$FLOW" check-contents setter_interface_missing_param.js < setter_interface_missing_param.js
queries_in_file type-at-pos setter_interface_missing_param.js --pretty
assert_errors "$FLOW" check-contents setter_interface_multiple_params.js < setter_interface_multiple_params.js
queries_in_file type-at-pos setter_interface_multiple_params.js --pretty

# Objects
assert_errors "$FLOW" check-contents setter_object_missing_param.js < setter_object_missing_param.js
queries_in_file type-at-pos setter_object_missing_param.js --pretty
assert_errors "$FLOW" check-contents setter_object_multiple_params.js < setter_object_multiple_params.js
queries_in_file type-at-pos setter_object_multiple_params.js --pretty

# Object types
assert_errors "$FLOW" check-contents setter_type_missing_param.js < setter_type_missing_param.js
queries_in_file type-at-pos setter_type_missing_param.js --pretty
assert_errors "$FLOW" check-contents setter_type_multiple_params.js < setter_type_multiple_params.js
queries_in_file type-at-pos setter_type_multiple_params.js --pretty
