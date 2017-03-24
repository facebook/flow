#!/bin/sh

FLOW=$1
printf "\nStop any already-running server."
$FLOW stop .

printf "\nFull check:\n"
$FLOW check .

printf "\nFocused check on a file with local errors but no dependency:\n"
$FLOW focus-check test.js

printf "\nFocused check on a file with no local errors but a dependency:\n"
$FLOW focus-check a.js

printf "\nFocused check on a file with no local errors and no dependency:\n"
$FLOW focus-check b.js

printf "\nDone!\n"
