#!/bin/bash

cd src || exit
assert_errors "$FLOW" check --strip-root
