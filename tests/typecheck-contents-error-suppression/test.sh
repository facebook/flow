#!/bin/bash
. ../assert.sh

FLOW=$1

assert_ok "$FLOW" check-contents test.js < test.js
