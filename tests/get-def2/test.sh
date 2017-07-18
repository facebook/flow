#!/bin/sh
. ../assert.sh
FLOW=$1

printf "\nTrace \`ParentFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 8 1
assert_ok "$FLOW" get-def --strip-root main.js 7 3
assert_ok "$FLOW" get-def --strip-root Parent.js 4 19

printf "\nTrace \`ParentFoo2\` back to its def (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 13 1
assert_ok "$FLOW" get-def --strip-root main.js 12 1
assert_ok "$FLOW" get-def --strip-root main.js 12 14

printf "\nTrace \`ParentFoo3\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 17 1

printf "\nTrace \`Parent\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 5
assert_ok "$FLOW" get-def --strip-root main.js 3 14

printf "\nTrace \`NonDestructuredFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 21 2
assert_ok "$FLOW" get-def --strip-root main.js 20 5
assert_ok "$FLOW" get-def --strip-root main.js 20 41

printf "\nReact class and attribute expressions\n"
assert_ok "$FLOW" get-def --strip-root react.js 9 3
assert_ok "$FLOW" get-def --strip-root react.js 9 9
assert_ok "$FLOW" get-def --strip-root react.js 11 4
assert_ok "$FLOW" get-def --strip-root react.js 11 12

printf "\nInheritance\n"
assert_ok "$FLOW" get-def --strip-root override.js 8 19

printf "\nDirectly jump to required/imported modules (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 26
assert_ok "$FLOW" get-def --strip-root main.js 23 29
assert_ok "$FLOW" get-def --strip-root main.js 23 14
assert_ok "$FLOW" get-def --strip-root main.js 24 25
