#!/bin/bash
. ../fs.sh

copy ./commit_a/import.js ./import.js
copy ./commit_a/test.js ./test.js
printf "Run flow on commit A\n"
assert_ok "$FLOW" status .

copy ./commit_b/import.js ./import.js
copy ./commit_b/test.js ./test.js
printf "\nRun flow on commit B\n"
assert_errors "$FLOW" status .

rm *.js
