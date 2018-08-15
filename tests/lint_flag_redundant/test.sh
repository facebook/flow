#!/bin/bash
assert_exit "$EXIT_USAGE" \
  "$FLOW" check . --all --lints "sketchy-null=error,sketchy-null-bool=error"
