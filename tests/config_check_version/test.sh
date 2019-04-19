#!/bin/bash

echo "flow config check:"
assert_exit 8 "$FLOW" config check --pretty

echo
echo "flow config check --ignore-version:"
assert_ok "$FLOW" config check --ignore-version --pretty
