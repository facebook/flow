#!/bin/bash

echo "FLOW STATUS:"

assert_errors $FLOW status

echo "FLOW CHECK-CONTENTS:"

assert_errors $FLOW check-contents bad-named-export.js < bad-named-export.js
