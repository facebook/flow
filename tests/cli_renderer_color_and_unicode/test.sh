#!/bin/bash

source ../assert.sh
assert_errors "$1" check . --all --no-flowlib --show-all-errors --include-warnings --color=always --unicode=always
