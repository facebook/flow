#!/bin/bash
echo "Don't crash on MergedT:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root test.js 7 3
