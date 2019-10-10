#!/bin/bash
printf "\nTrace \`ParentFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 8 1
assert_ok "$FLOW" get-def --strip-root main.js 7 3
assert_ok "$FLOW" get-def --strip-root Parent.js 4 19

printf "\nTrace \`Parent2\` back to its def (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 13 1
assert_ok "$FLOW" get-def --strip-root main.js 12 1
assert_ok "$FLOW" get-def --strip-root main.js 12 14

printf "\nTrace \`Parent3\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 17 1

printf "\nTrace \`Parent\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 5
assert_ok "$FLOW" get-def --strip-root main.js 3 14

printf "\nTrace \`NonDestructuredFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 21 2
assert_ok "$FLOW" get-def --strip-root main.js 20 5
assert_ok "$FLOW" get-def --strip-root main.js 20 41

printf "\nTrace \`ParentFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root require.js 4 2

printf "\nReact class and attribute expressions\n"
assert_ok "$FLOW" get-def --strip-root react.js 9 3
assert_ok "$FLOW" get-def --strip-root react.js 9 9
assert_ok "$FLOW" get-def --strip-root react.js 11 4
assert_ok "$FLOW" get-def --strip-root react.js 11 12
# TODO give some result for the JSX intrinsic here
assert_ok "$FLOW" get-def --strip-root react.js 11 4

printf "\nInheritance\n"
assert_ok "$FLOW" get-def --strip-root override.js 8 19
assert_ok "$FLOW" get-def --strip-root override.js 11 11
assert_ok "$FLOW" get-def --strip-root override.js 12 11

printf "\nDirectly jump to required/imported modules (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 26
assert_ok "$FLOW" get-def --strip-root main.js 23 29
assert_ok "$FLOW" get-def --strip-root main.js 23 14
assert_ok "$FLOW" get-def --strip-root main.js 24 25

printf "\nRefinements\n"
assert_ok "$FLOW" get-def --strip-root refinements.js 10 9
assert_ok "$FLOW" get-def --strip-root refinements.js 11 9

printf "\ndeclare var\n"
assert_ok "$FLOW" get-def --strip-root declare.js 4 1
assert_ok "$FLOW" get-def --strip-root declare.js 7 1
assert_ok "$FLOW" get-def --strip-root declare.js 10 1
assert_ok "$FLOW" get-def --strip-root declare.js 13 5

printf "\nShadowing \`require\`\n"
assert_ok "$FLOW" get-def --strip-root main.js 28 20
assert_ok "$FLOW" get-def --strip-root main.js 29 6
assert_ok "$FLOW" get-def --strip-root main.js 33 3

printf "\nChaining \`require\`s through multiple files\n"
assert_ok "$FLOW" get-def --strip-root main.js 33 10
assert_ok "$FLOW" get-def --strip-root main.js 33 17
