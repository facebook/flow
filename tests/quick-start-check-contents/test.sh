#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

printf "\nQuick start.\n"
$FLOW start --lazy .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start

printf "\nCheck contents with dependency, expect errors in contents.\n"
$FLOW check-contents --no-auto-start test.js < test.js

printf "\nExpect errors in dependency.\n"
$FLOW status --no-auto-start

printf "\nUnsaved files don't have dependents, as in non-lazy mode.\n"
$FLOW check-contents --no-auto-start foo.js < unsaved_foo.js

printf "\nDone!\n"
