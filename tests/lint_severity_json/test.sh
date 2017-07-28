#!/bin/bash
. ../assert.sh
FLOW=$1
assert_errors "$FLOW" check . --all --include-warnings --pretty --strip-root \
  | grep -v '^ *"flowVersion":.*'
