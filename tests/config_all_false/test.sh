#!/bin/bash
. ../assert.sh
FLOW=$1
assert_ok "$FLOW" check . --strip-root --show-all-errors
