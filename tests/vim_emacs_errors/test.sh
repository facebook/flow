#!/bin/sh

FLOW="$1"

printf "from emacs:\n"
"$FLOW" status --strip-root --from emacs

printf "from vim:\n"
"$FLOW" status --strip-root --from vim
