#!/bin/bash
. ../assert.sh
FLOW=$1

echo "Simple object property multi-hop find-refs:"
assert_ok "$FLOW" find-refs --multi-hop --json --pretty --strip-root usesFoo.js 6 5
