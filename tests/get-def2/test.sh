#!/bin/sh

FLOW=$1

# Trace `ParentFoo` back to its def
$FLOW get-def --strip-root main.js 8 1
$FLOW get-def --strip-root main.js 7 3
$FLOW get-def --strip-root Parent.js 4 19

# Trace `ParentFoo2` back to its def
$FLOW get-def --strip-root main.js 13 1
$FLOW get-def --strip-root main.js 12 1
$FLOW get-def --strip-root main.js 12 14

# Trace `ParentFoo3` back to its def
$FLOW get-def --strip-root main.js 17 1

# Trace `Parent` back to its def
$FLOW get-def --strip-root main.js 3 5
$FLOW get-def --strip-root main.js 3 14

# Trace `NonDestructuredFoo` back to its def
$FLOW get-def --strip-root main.js 21 2
$FLOW get-def --strip-root main.js 20 5
$FLOW get-def --strip-root main.js 20 41

# React class and attribute expressions
$FLOW get-def --strip-root react.js 9 3
$FLOW get-def --strip-root react.js 9 9
$FLOW get-def --strip-root react.js 11 4
$FLOW get-def --strip-root react.js 11 12

# Inheritance
$FLOW get-def --strip-root override.js 8 19

# Directly jump to required/imported modules
$FLOW get-def --strip-root main.js 3 26
$FLOW get-def --strip-root main.js 23 29
