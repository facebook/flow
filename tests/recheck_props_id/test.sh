#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

show_obj_obj_fast_path() {
  grep "ObjT ~> ObjT fast path" $1 | tail -n 1
}

mkdir tmp
cp a.js tmp/

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose

assert_ok "$FLOW" status --strip-root
printf "\\nA self-subtyping check in a downstream file for an object defined upstream should go through a fast path\\n"
show_obj_obj_fast_path "$FLOW_LOG_FILE"

printf "\\nAn upstream edit that does not touch that object should preserve the fast path\\n"
cp tmp1/a.js a.js
assert_ok "$FLOW" force-recheck --focus a.js
assert_ok "$FLOW" status --strip-root
show_obj_obj_fast_path "$FLOW_LOG_FILE"

assert_ok "$FLOW" stop

mv tmp/a.js a.js
rmdir tmp
