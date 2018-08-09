#!/bin/bash
assert_errors "$FLOW" check . --all --lints "all=error"
