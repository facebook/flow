#!/bin/sh

FLOW=$1

printf "\nTrace \`ParentFoo\` back to its def\n"
$FLOW get-def --strip-root main.js 8 1
$FLOW get-def --strip-root main.js 7 3
$FLOW get-def --strip-root Parent.js 4 19

printf "\nTrace \`ParentFoo2\` back to its def (TODO #2)\n"
$FLOW get-def --strip-root main.js 13 1
$FLOW get-def --strip-root main.js 12 1
$FLOW get-def --strip-root main.js 12 14

printf "\nTrace \`ParentFoo3\` back to its def\n"
$FLOW get-def --strip-root main.js 17 1

printf "\nTrace \`Parent\` back to its def\n"
$FLOW get-def --strip-root main.js 3 5
$FLOW get-def --strip-root main.js 3 14

printf "\nTrace \`NonDestructuredFoo\` back to its def\n"
$FLOW get-def --strip-root main.js 21 2
$FLOW get-def --strip-root main.js 20 5
$FLOW get-def --strip-root main.js 20 41

printf "\nReact class and attribute expressions\n"
$FLOW get-def --strip-root react.js 9 3
$FLOW get-def --strip-root react.js 9 9
$FLOW get-def --strip-root react.js 11 4
$FLOW get-def --strip-root react.js 11 12

printf "\nInheritance\n"
$FLOW get-def --strip-root override.js 8 19

printf "\nDirectly jump to required/imported modules (TODO #2)\n"
$FLOW get-def --strip-root main.js 3 26
$FLOW get-def --strip-root main.js 23 29
$FLOW get-def --strip-root main.js 23 14
