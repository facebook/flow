#!/bin/bash
FLOW=$1

$FLOW dump-types --strip-root --json test.js
$FLOW dump-types --strip-root --raw test.js
