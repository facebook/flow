#!/bin/sh

FLOW=$1

printf "\nInitial status:\n"
$FLOW status --no-auto-start --strip-root .

printf "\nMove A.js to B.js (blacklisted):\n"
mv A.js B.js
$FLOW force-recheck --no-auto-start A.js B.js
$FLOW status --no-auto-start --strip-root .

printf "\nMove B.js to A.js:\n"
mv B.js A.js
$FLOW force-recheck --no-auto-start A.js B.js
$FLOW status --no-auto-start --strip-root .

printf "\nDone!\n"
