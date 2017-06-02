#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

mkdir tmp

printf "\nQuick start.\n"
$FLOW start --lazy .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start .

printf "\nDelete @flow file with a @flow dependent file, expect error.\n"
mv a.js tmp/
$FLOW force-recheck a.js
$FLOW status --no-auto-start .

printf "\nRevert file, expect no errors.\n"
mv tmp/a.js .
$FLOW force-recheck a.js
$FLOW status --no-auto-start .

rm -rf tmp
printf "\nDone!\n"
