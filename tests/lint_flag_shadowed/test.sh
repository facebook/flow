#!/bin/bash
FLOW=$1
"$FLOW" check . --all --lints "sketchy-null-bool=error,sketchy-null=off"
