#!/bin/sh

FLOW=$1

printf "After start:\n"
$FLOW status .

cp test1.js.fixture test.js
printf "\nAfter introducing a lint error:\n"
$FLOW force-recheck test.js
$FLOW status .

cp test2.js.fixture test.js
printf "\nAfter suppressing the lint error:\n"
$FLOW force-recheck test.js
$FLOW status .

cp test3.js.fixture test.js
printf "\nAfter fixing the lint error, leaving the suppression:\n"
printf "(TODO: check for unused lint suppressions)\n"
$FLOW force-recheck test.js
$FLOW status .

cp test4.js.fixture test.js
printf "\nAfter removing the unused suppression:\n"
$FLOW force-recheck test.js
$FLOW status .
