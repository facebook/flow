#!/bin/bash
assert_errors "$FLOW" check . --all --lints "sketchy-null=error"
