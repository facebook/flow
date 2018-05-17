#!/bin/bash
. ../assert.sh

FLOW=$1

rm .flowconfig
assert_ok "$FLOW" init

printf "Default .flowconfig should typecheck:\n"
assert_ok "$FLOW" check

printf "\nDefault .flowconfig looks like this:\n"
cat .flowconfig
