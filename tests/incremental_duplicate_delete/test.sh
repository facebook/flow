#!/bin/sh

FLOW=$1

printf "\nInitial status:\n"
$FLOW status --no-auto-start --strip-root .

printf "\nCopy A.js to B.js:\n"
cp A.js B.js
$FLOW force-recheck --no-auto-start B.js
$FLOW status --no-auto-start --strip-root .

printf "\nDelete A.js:\n"
rm A.js
$FLOW force-recheck --no-auto-start A.js
$FLOW status --no-auto-start --strip-root .

printf "\nMove B.js to A.js:\n"
mv B.js A.js
$FLOW force-recheck --no-auto-start A.js B.js
$FLOW status --no-auto-start --strip-root .

printf "\nDone!\n"
