#!/bin/sh
FLOW=$1
$FLOW stop 2> /dev/null > /dev/null
mkdir -p tmp/flow
$FLOW start --wait --temp-dir tmp/flow
if [[ "$OSTYPE" == "msys"* ]]; then
    [ -f tmp/flow/*.sock ]    && echo "sock file exists"
else
    [ -S tmp/flow/*.sock ]    && echo "sock file exists"
fi
[ -d tmp/flow/flowlib_* ] && echo "flowlib exists"
[ -f tmp/flow/*.init ]    && echo "init file exists"
[ -f tmp/flow/*.lock ]    && echo "lock file exists"
[ -f tmp/flow/*.log ]     && echo "log file exists"
# Stop the server before removing the tmp dir
$FLOW stop 2> /dev/null > /dev/null
rm -rf tmp
