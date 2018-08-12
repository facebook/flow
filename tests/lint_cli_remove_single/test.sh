#!/bin/bash
assert_ok "$FLOW" check . --all --lints "sketchy-null=off"
