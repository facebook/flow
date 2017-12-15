#!/bin/bash
. ../assert.sh
FLOW=$1
assert_errors "$FLOW" status --include-warnings --strip-root --from vim
