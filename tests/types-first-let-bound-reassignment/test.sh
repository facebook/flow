#!/bin/bash

printf "\\nErrors in classic\\n"
assert_errors "$FLOW" check --strip-root --no-flowlib

printf "\\nErrors in types-first\\n"
assert_errors "$FLOW" check --strip-root --no-flowlib --types-first
