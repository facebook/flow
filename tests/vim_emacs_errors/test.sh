#!/bin/bash
. ../assert.sh
FLOW="$1"

printf "from emacs:\n"
assert_errors "$FLOW" status --strip-root --from emacs

printf "from vim:\n"
assert_errors "$FLOW" status --strip-root --from vim
