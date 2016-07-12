#!/bin/bash
FLOW=$1

$FLOW dump-types --strip-root --json --pretty test.js
$FLOW dump-types --strip-root --raw --pretty test.js
