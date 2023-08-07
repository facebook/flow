#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file get-def "class.js"
queries_in_file get-def "declare_class.js"
queries_in_file get-def "function.js"
queries_in_file get-def "interface.js"
queries_in_file get-def "opaque_type_alias.js"
queries_in_file get-def "type_alias.js"
