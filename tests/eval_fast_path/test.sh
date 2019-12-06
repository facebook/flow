#!/bin/bash

show_eval_eval_fast_path() {
  grep "EvalT ~> EvalT fast path" $1 | tail -n 1
}

printf "\\nServer should start in verbose mode\\n"
start_flow . --verbose
assert_ok "$FLOW" status --strip-root
show_eval_eval_fast_path "$FLOW_LOG_FILE"
