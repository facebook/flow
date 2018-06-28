#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nRename of an object property:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 3 21 --rename newName

printf "\nRename of a local variable used in object shorthand:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 6 9 --rename newName

printf "\nRename of a local variable bound by destructuring shorthand:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 12 10 --rename newName
