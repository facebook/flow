#!/bin/sh
FLOW=$1
$FLOW stop 2> /dev/null > /dev/null
DIR=$(mktemp -d /tmp/flow.XXXXX)
$FLOW start --wait --temp-dir "$DIR"
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
