#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

printf "\nQuick start.\n"
$FLOW start --lazy .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start .

printf "\nTouch @flow file, expect error.\n"
touch a.js
$FLOW force-recheck a.js
$FLOW status --no-auto-start .

printf "\nMake file @noflow, expect error to go away.\n"
cp tmp1/a.js a.js
$FLOW force-recheck a.js
$FLOW status --no-auto-start .

printf "\nRevert file, expect error again.\n"
cp tmp2/a.js a.js
$FLOW force-recheck a.js
$FLOW status --no-auto-start .

printf "\nDone!\n"
