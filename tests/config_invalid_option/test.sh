#!/bin/bash

echo "Invalid flowconfig should fail"
assert_exit 8 "$FLOW" check 2>&1

echo "Invalid flowconfig should pass"
assert_ok "$FLOW" check --quiet --ignore-version 2>&1
