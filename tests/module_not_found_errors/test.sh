#!/bin/bash
. ../assert.sh
FLOW=$1
cd src || exit
assert_errors "$FLOW" check --strip-root
