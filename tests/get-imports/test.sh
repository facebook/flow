#!/bin/sh
FLOW=$1

$FLOW get-imports --strip-root --json a.js
$FLOW get-imports --strip-root --json b
$FLOW get-imports --strip-root --json b.js
$FLOW get-imports --strip-root --json c.js
