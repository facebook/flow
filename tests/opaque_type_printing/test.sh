#!/bin/sh
assert_ok \
  "$FLOW" type-at-pos test.js 5 12 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos importtest.js 5 12 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos importtest.js 9 12 --strip-root --pretty
