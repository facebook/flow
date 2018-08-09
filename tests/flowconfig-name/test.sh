#!/bin/bash

assert_ok "$FLOW" stop
assert_ok "$FLOW" stop --flowconfig-name configure-flow

printf "\\nWe should use the all flag from configure-flow to find an error:\\n"
assert_errors "$FLOW" check --flowconfig-name configure-flow

printf "\\nWe can start a server using the configure-flow file:\\n"
assert_ok "$FLOW" start --flowconfig-name configure-flow

printf "\\nAnd still find the error with flow status!:\\n"
assert_errors "$FLOW" status --flowconfig-name configure-flow

printf "\\nWe can simultaneously start a server with the regular .flowconfig:\\n"
assert_ok "$FLOW" start

printf "\\nAnd still find the error with the original configure-flow config:\\n"
assert_errors "$FLOW" status --flowconfig-name configure-flow

printf "\\nAnd not check any non @flow files using the .flowconfig config:\\n"
assert_ok "$FLOW" status
