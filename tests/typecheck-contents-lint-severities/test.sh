#!/bin/bash
. ../assert.sh

FLOW=$1

assert_errors "$FLOW" check-contents test.js < test.js
