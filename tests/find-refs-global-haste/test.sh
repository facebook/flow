#!/bin/bash
echo "simple ES6 exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 6 17
