#!/bin/bash
printf "\nCheck without --all doesn't read malformed @flow file\n"
assert_ok "$FLOW" check --strip-root .

printf "\nCheck with --all sees a parse error\n"
assert_errors "$FLOW" check --all --strip-root .
