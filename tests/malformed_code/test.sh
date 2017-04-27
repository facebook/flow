#!/bin/sh

FLOW=$1
printf "\nCheck without --all doesn't read malformed @flow file\n"
$FLOW check --strip-root .

printf "\nCheck with --all sees a parse error\n"
$FLOW check --all --strip-root .
