#!/bin/bash
FLOW=$1
"$FLOW" start . --all
"$FLOW" status --strip-root --from vim
"$FLOW" stop
