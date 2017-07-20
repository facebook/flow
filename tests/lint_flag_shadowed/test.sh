#!/bin/bash
. ../assert.sh
FLOW=$1

assert_exit "$EXIT_USAGE" \
  "$FLOW" check . --all --lints "sketchy-null-bool=error,sketchy-null=off"
