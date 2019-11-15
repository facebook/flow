#!/bin/bash
. ../fs.sh

"$FLOW" stop
"$FLOW" start --lazy-mode none

cp ./commit_a/*.js .

printf "Run flow on commit A\n"
assert_ok "$FLOW" status .

cp ./commit_b/*.js .

printf "\nRun flow on commit B\n"
assert_ok "$FLOW" status .

rm *.js
