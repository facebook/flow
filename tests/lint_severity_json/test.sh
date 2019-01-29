#!/bin/bash
assert_errors "$FLOW" check . --all --include-warnings --pretty --strip-root \
  | grep -v '^ *"flowVersion":.*'
