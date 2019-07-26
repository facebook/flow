#!/bin/bash

show_obj_obj_fast_path() {
  grep "ObjT ~> ObjT fast path" $1 | tail -n 1
}

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose
assert_ok "$FLOW" status --strip-root
show_obj_obj_fast_path "$FLOW_LOG_FILE"
