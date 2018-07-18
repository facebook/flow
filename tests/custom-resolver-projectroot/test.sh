#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" status .
