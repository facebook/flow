#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

show_union_union_slow_path() {
  grep "UnionT ~> UnionT slow case" $1 | tail -n 7
}

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose
assert_ok "$FLOW" status --strip-root
show_union_union_slow_path "$FLOW_LOG_FILE"
