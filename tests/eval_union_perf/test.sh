#!/bin/bash

show_union_union_slow_path() {
  grep "UnionT ~> UnionT slow case" $1 | tail -n 7
}

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose
assert_ok "$FLOW" status --strip-root
show_union_union_slow_path "$FLOW_LOG_FILE"
