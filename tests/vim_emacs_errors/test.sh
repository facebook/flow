#!/bin/bash

printf "from emacs:\n"
assert_errors "$FLOW" status --strip-root --from emacs

printf "from vim:\n"
assert_errors "$FLOW" status --strip-root --from vim
