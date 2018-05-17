#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" type-at-pos printBinaryExpression.js 15 12 --strip-root
assert_ok "$FLOW" type-at-pos printBinaryExpression.js 17 15 --strip-root
