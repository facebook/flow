#!/bin/bash
FLOW=$1
"$FLOW" check . --all --lints "all=on"
