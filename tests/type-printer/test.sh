#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" type-at-pos printBinaryExpression.js 17 15 --raw --strip-root
