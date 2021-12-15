#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose-indent
assert_ok "$FLOW" status --strip-root
assert_ok grep "union_compare slow" "$FLOW_LOG_FILE" # TODO
