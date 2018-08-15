#!/bin/bash
assert_ok "$FLOW" stop 2> /dev/null > /dev/null
for i in $(seq 1 20); do
  assert_ok "$FLOW" status 2> /dev/null > /dev/null &
done
assert_ok "$FLOW" status
