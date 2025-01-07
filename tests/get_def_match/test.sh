#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "get-def" "bindings-introduced.js"
queries_in_file "get-def" "pattern-identifier.js"
queries_in_file "get-def" "pattern-member.js"
queries_in_file "get-def" "pattern-object.js"
