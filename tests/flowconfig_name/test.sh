#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop
assert_ok "$FLOW" stop --flowconfig-name configure-flow

printf "\\nWe should use the all flag from configure-flow to find an error:\\n"
assert_errors "$FLOW" check --flowconfig-name configure-flow

printf "\\nWe can start a server using the configure-flow file:\\n"
start_flow . --flowconfig-name configure-flow

printf "\\nAnd still find the error with flow status!:\\n"
assert_errors "$FLOW" status --flowconfig-name configure-flow

printf "\\nWe can simultaneously start a server with the regular .flowconfig:\\n"
start_flow .

printf "\\nAnd still find the error with the original configure-flow config:\\n"
assert_errors "$FLOW" status --flowconfig-name configure-flow

printf "\\nAnd not check any non @flow files using the .flowconfig config:\\n"
assert_ok "$FLOW" status

assert_ok "$FLOW" stop
assert_ok "$FLOW" stop --flowconfig-name configure-flow

printf "\\nWe should be able to run status without start:\\n"
assert_errors "$FLOW" status --flowconfig-name configure-flow

assert_ok "$FLOW" stop --flowconfig-name configure-flow

printf "\\nWe should be able to run without any command specified:\\n"
assert_errors "$FLOW" --flowconfig-name configure-flow

assert_ok "$FLOW" stop --flowconfig-name configure-flow
