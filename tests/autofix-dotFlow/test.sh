#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 3 20
echo "> cat a.js"
cat a.js

assert_ok "$FLOW" autofix insert-type --in-place a.js.flow 3 20
echo "> cat a.js.flow"
cat a.js.flow

assert_ok "$FLOW" force-recheck a.js a.js.flow
echo "> flow status"
assert_ok "$FLOW" status
