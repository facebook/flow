#!/bin/bash

echo "FLOW STATUS:"

# Has an unrelated error in the lib file
assert_errors $FLOW status

echo "FLOW CHECK-CONTENTS:"

# Has an unrelated error in the lib file
assert_errors $FLOW check-contents bad-default-export.js < bad-default-export.js
