#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" stop 2> /dev/null > /dev/null

# These are set by runtests.sh
unset FLOW_TEMP_DIR
unset FLOW_LOG_FILE
unset FLOW_MONITOR_LOG_FILE

echo "--temp-dir:"
DIR=$(mktemp -d /tmp/flow.XXXXXX)
assert_ok "$FLOW" start --wait --temp-dir "$DIR" 2> /dev/null > /dev/null
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
else
    [ -S "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
fi
[ -d "$DIR"/flowlib_* ]     && echo "  flowlib exists"
[ -f "$DIR"/*.lock ]        && echo "  lock file exists"
[ -f "$DIR"/*.log ]         && echo "  log file exists"
[ -f "$DIR"/*.monitor_log ] && echo "  monitor log file exists"
# Stop the server before removing the tmp dir
assert_ok "$FLOW" stop --temp-dir "$DIR" 2> /dev/null > /dev/null
rm -rf "$DIR"
echo

# Test a .flowconfig with temp_dir
echo ".flowconfig temp_dir:"
DIR=$(mktemp -d /tmp/flow.XXXXXX)
TEST_DIR=$(mktemp -d /tmp/flow.XXXXXX)
printf "[options]\ntemp_dir=%s" "$DIR" > "$TEST_DIR/.flowconfig"
assert_ok "$FLOW" status "$TEST_DIR" 2> /dev/null > /dev/null
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f "$DIR"/*.sockv2 ]    && echo "  sockv2 file exists"
else
    [ -S "$DIR"/*.sockv2 ]    && echo "  sockv2 file exists"
fi
assert_ok "$FLOW" stop "$TEST_DIR" 2> /dev/null > /dev/null
rm -rf "$TEST_DIR"
rm -rf "$DIR"
echo

# Test FLOW_TEMP_DIR
echo "FLOW_TEMP_DIR:"
DIR=$(mktemp -d /tmp/flow.XXXXXX)
export FLOW_TEMP_DIR="$DIR"
assert_ok "$FLOW" start --wait 2> /dev/null > /dev/null
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
else
    [ -S "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
fi
[ -d "$DIR"/flowlib_* ]     && echo "  flowlib exists"
[ -f "$DIR"/*.lock ]        && echo "  lock file exists"
[ -f "$DIR"/*.log ]         && echo "  log file exists"
[ -f "$DIR"/*.monitor_log ] && echo "  monitor log file exists"
# Stop the server before removing the tmp dir
assert_ok "$FLOW" stop 2> /dev/null > /dev/null
rm -rf "$DIR"
