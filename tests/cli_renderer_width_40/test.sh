#!/bin/bash

assert_errors "$1" check . --all --no-flowlib --show-all-errors --message-width=40
