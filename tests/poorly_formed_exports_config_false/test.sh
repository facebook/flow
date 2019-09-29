#!/bin/bash

echo "FLOW STATUS:"

assert_ok $FLOW status

echo "FLOW CHECK-CONTENTS:"

assert_ok $FLOW check-contents bad-default-export.js < bad-default-export.js
