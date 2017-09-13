#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nVariable defs and uses:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 4 5
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 5 2

printf "\nNested functions:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 10 10
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 13 3

printf "\nClasses:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 18 7

# printf "\nType aliases:\n"
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 23 6

printf "\nRefinements:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 28 6
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 29 6
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 30 16
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 31 8
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 33 2

printf "\nDestructuring:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 36 7
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 37 10
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 37 26
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 38 7

# printf "\nNot in scope:\n"
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 41 2
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 42 2
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 42 9
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 43 2

# printf "\nJSX:\n"
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 50 4

printf "\nImports:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 55 2
# This is a type, which doesn't work yet
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 55 9

# printf "\nQualified types:\n"
# assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 58 9

printf "\nExports:\n"
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 64 20
assert_ok "$FLOW" find-refs --json --pretty --strip-root locals.js 65 6
