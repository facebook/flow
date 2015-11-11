#!/bin/sh
FLOW=$1
$FLOW stop 2> /dev/null > /dev/null
mkdir -p tmp/flow
$FLOW start --wait --temp-dir tmp/flow
[ -S tmp/flow/*.sock ]    && echo "sock file exists"
[ -d tmp/flow/flowlib_* ] && echo "flowlib exists"
[ -f tmp/flow/*.init ]    && echo "init file exists"
[ -f tmp/flow/*.lock ]    && echo "lock file exists"
[ -f tmp/flow/*.log ]     && echo "log file exists"
rm -rf tmp/flow
