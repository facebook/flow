#!/bin/bash
FLOW=$1
"$FLOW" check . --strip-root --show-all-errors
