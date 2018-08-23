#!/bin/bash

assert_errors "$1" check . --all --no-flowlib --include-warnings
