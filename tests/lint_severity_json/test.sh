#!/bin/bash
FLOW=$1
"$FLOW" check . --all --pretty --strip-root \
  | grep -v '^ *"flowVersion":.*'
