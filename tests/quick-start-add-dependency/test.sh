#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

printf "\nQuick start.\n"
$FLOW quick-start .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start .

printf "\nTouch @flow file with dependency on a @flow file, expect no error.\n"
touch b.js
$FLOW force-recheck b.js
$FLOW status --no-auto-start .

printf "\nDone!\n"
