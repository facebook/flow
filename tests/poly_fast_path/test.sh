#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

show_poly_poly_fast_path() {
  grep "PolyT ~> PolyT fast path" $1 | tail -n 1
}

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose
assert_ok "$FLOW" status --strip-root
show_poly_poly_fast_path "$FLOW_LOG_FILE"
