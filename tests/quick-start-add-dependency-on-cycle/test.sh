#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

printf "\nQuick start.\n"
$FLOW start --lazy .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start .

printf "\nTouch @flow file with dependency on a @flow file in a cycle, expect no error.\n"
touch c.js
$FLOW force-recheck c.js
$FLOW status --no-auto-start .

printf "\nDone!\n"
