#!/bin/sh

FLOW=$1

printf "\nVariable defs and uses:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 4 5
$FLOW find-refs --json --pretty --strip-root locals.js 5 2

printf "\nNested functions:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 10 10
$FLOW find-refs --json --pretty --strip-root locals.js 13 3

printf "\nClasses:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 18 7

printf "\nType aliases:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 23 6

printf "\nRefinements:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 28 6
$FLOW find-refs --json --pretty --strip-root locals.js 29 6
$FLOW find-refs --json --pretty --strip-root locals.js 30 16
$FLOW find-refs --json --pretty --strip-root locals.js 31 8
$FLOW find-refs --json --pretty --strip-root locals.js 33 2

printf "\nDestructuring:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 36 7
$FLOW find-refs --json --pretty --strip-root locals.js 37 10
$FLOW find-refs --json --pretty --strip-root locals.js 37 26
$FLOW find-refs --json --pretty --strip-root locals.js 38 7

printf "\nNot in scope:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 41 2
$FLOW find-refs --json --pretty --strip-root locals.js 42 2
$FLOW find-refs --json --pretty --strip-root locals.js 42 9
$FLOW find-refs --json --pretty --strip-root locals.js 43 2

printf "\nJSX:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 50 4

printf "\nImports:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 55 2
$FLOW find-refs --json --pretty --strip-root locals.js 55 9

printf "\nQualified types:\n"
$FLOW find-refs --json --pretty --strip-root locals.js 58 9
