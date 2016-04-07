#!/bin/bash
FLOW=$1
$FLOW stop 2> /dev/null > /dev/null
DIR=$(mktemp -d /tmp/flow.XXXXXX)
$FLOW start --wait --temp-dir "$DIR" 2> /dev/null > /dev/null
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f "$DIR"/*.sock ]    && echo "sock file exists"
else
    [ -S "$DIR"/*.sock ]    && echo "sock file exists"
fi
[ -d "$DIR"/flowlib_* ] && echo "flowlib exists"
[ -f "$DIR"/*.init ]    && echo "init file exists"
[ -f "$DIR"/*.lock ]    && echo "lock file exists"
[ -f "$DIR"/*.log ]     && echo "log file exists"
# Stop the server before removing the tmp dir
$FLOW stop --temp-dir "$DIR" 2> /dev/null > /dev/null
rm -rf "$DIR"

# Test a .flowconfig with temp_dir
DIR=$(mktemp -d /tmp/flow.XXXXXX)
TEST_DIR=$(mktemp -d /tmp/flow.XXXXXX)
printf "[options]\ntemp_dir=%s" "$DIR" > "$TEST_DIR/.flowconfig"
$FLOW status "$TEST_DIR" 2> /dev/null > /dev/null
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f "$DIR"/*.sock ]    && echo ".flowconfig: sock file exists"
else
    [ -S "$DIR"/*.sock ]    && echo ".flowconfig: sock file exists"
fi
$FLOW stop "$TEST_DIR" 2> /dev/null > /dev/null
rm -rf "$TEST_DIR"
rm -rf "$DIR"
