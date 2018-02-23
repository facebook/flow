#!/bin/bash

source ../assert.sh
assert_errors "$1" check . --all --no-flowlib --show-all-errors --unicode=always
