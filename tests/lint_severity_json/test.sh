#!/bin/bash
. ../assert.sh
FLOW=$1

assert_errors "$FLOW" check . --all --pretty --strip-root \
  | grep -v '^ *"flowVersion":.*'
