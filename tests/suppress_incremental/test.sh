#!/bin/sh

FLOW=$1

printf "After start:\n"
$FLOW status . --old-output-format

cp test1.js.fixture test.js
printf "\nAfter introducing an error:\n"
$FLOW force-recheck test.js
$FLOW status . --old-output-format

cp test2.js.fixture test.js
printf "\nAfter suppressing the error:\n"
$FLOW force-recheck test.js
$FLOW status . --old-output-format

cp test3.js.fixture test.js
printf "\nAfter fixing the error, leaving the suppression:\n"
$FLOW force-recheck test.js
$FLOW status . --old-output-format

cp test4.js.fixture test.js
printf "\nAfter removing the unused suppression:\n"
$FLOW force-recheck test.js
$FLOW status . --old-output-format
