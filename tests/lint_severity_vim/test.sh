#!/bin/bash
. ../assert.sh
FLOW=$1
assert_ok "$FLOW" stop
assert_ok "$FLOW" start . --all
assert_errors "$FLOW" status --strip-root --from vim
