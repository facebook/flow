#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2144

assert_ok "$FLOW" stop 2> /dev/null > /dev/null

# These are set by runtests.sh
TEST_TMP_DIR=${FLOW_TEMP_DIR:-${TMPDIR:-${TEMP:-${TMP:-/tmp}}}}
unset FLOW_TEMP_DIR
unset FLOW_LOG_FILE
unset FLOW_MONITOR_LOG_FILE

if command -v cygpath > /dev/null 2>&1; then
    TEST_TMP_DIR=$(cygpath -u "$TEST_TMP_DIR")
fi

make_temp_dir() {
    mktemp -d "$TEST_TMP_DIR/flow.XXXXXX"
}

flow_path() {
    if command -v cygpath > /dev/null 2>&1; then
        cygpath -m "$1"
    else
        printf "%s\n" "$1"
    fi
}

echo "--temp-dir:"
DIR=$(make_temp_dir)
FLOW_DIR=$(flow_path "$DIR")
assert_ok "$FLOW" start --wait --temp-dir "$FLOW_DIR" 2> /dev/null > /dev/null
# Windows stores local IPC connection info in a regular sockv2 marker file.
# Unix creates a Unix-domain socket at the sockv2 path.
if [[ "$OSTYPE" == "msys"* || "$OSTYPE" == "cygwin"* || "$OS" == "Windows_NT" ]]; then
    [ -f "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
else
    [ -S "$DIR"/*.sockv2 ]  && echo "  sockv2 file exists"
fi
[ -d "$DIR"/flowlib_* ]     && echo "  flowlib exists"
[ -f "$DIR"/*.lock ]        && echo "  lock file exists"
[ -f "$DIR"/*.log ]         && echo "  log file exists"
[ -f "$DIR"/*.monitor_log ] && echo "  monitor log file exists"
# Stop the server before removing the tmp dir
assert_ok "$FLOW" stop --temp-dir "$FLOW_DIR" 2> /dev/null > /dev/null
rm -rf "$DIR"
echo

# Test FLOW_TEMP_DIR
echo "FLOW_TEMP_DIR:"
DIR=$(make_temp_dir)
FLOW_DIR=$(flow_path "$DIR")
export FLOW_TEMP_DIR="$FLOW_DIR"
assert_ok "$FLOW" start --wait 2> /dev/null > /dev/null
# Windows stores local IPC connection info in a regular sockv2 marker file.
# Unix creates a Unix-domain socket at the sockv2 path.
if [[ "$OSTYPE" == "msys"* || "$OSTYPE" == "cygwin"* || "$OS" == "Windows_NT" ]]; then
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
