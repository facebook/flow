#!/bin/bash

mkdir tmp || rm -r tmp/*
cp -r .flowconfig lib tmp
start_flow tmp

cp a.js tmp/a.js
assert_ok "$FLOW" autofix insert-type --in-place a.js 6 18 6 23
assert_ok "$FLOW" force-recheck a.js
echo "> cat a.js"
cat a.js

assert_ok "$FLOW" autofix exports --in-place tmp/a.js
assert_ok "$FLOW" force-recheck tmp/a.js
echo "> cat tmp/a.js"
cat tmp/a.js

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
