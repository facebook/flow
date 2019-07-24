#!/bin/bash

printf "a.js:1:16 = "
assert_ok "$FLOW" type-at-pos a.js 1 16 --strip-root --pretty

printf "b.js:3:16 = "
assert_ok "$FLOW" type-at-pos b.js 3 16 --strip-root --pretty

printf "c.js:1:16 = "
assert_ok "$FLOW" type-at-pos c.js 1 16 --strip-root --pretty
