#!/bin/sh

FLOW=$1
printf "\nStop any already-running server.\n"
$FLOW stop .

printf "\nQuick start.\n"
$FLOW quick-start .

printf "\nExpect no errors.\n"
$FLOW status --no-auto-start .

printf "\n\"Warm up\" a dependency.\n"
touch b.js
$FLOW force-recheck b.js
$FLOW status --no-auto-start .

printf "\n\"Perturb\" a dependent that depends on the warmed up, but unchanged dependency.\n"
touch e.js
$FLOW force-recheck e.js
$FLOW status --no-auto-start .

printf "\nTest that the perturbed dependent is actually known (expecting a crash otherwise).\n"
touch d.js
$FLOW force-recheck d.js
$FLOW status --no-auto-start .

printf "\nDone!\n"
