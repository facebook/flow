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
