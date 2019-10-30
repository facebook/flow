#!/bin/bash

printf "\nFlowconfig should not be valid:\n";
assert_exit 8 "$FLOW" check --strip-root --no-flowlib . 2>&1
