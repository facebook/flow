#!/bin/bash
. ../assert.sh
FLOW=$1
assert_ok "$FLOW" check . --all --lints "all=off"
