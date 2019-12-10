#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nInvoking status with no offset-style argument:\n"
assert_errors "$FLOW" status --pretty --strip-root

printf "\nInvoking status with utf8-bytes offset-style:\n"
assert_errors "$FLOW" status --pretty --strip-root --offset-style utf8-bytes

printf "\nInvoking status with js-indices offset-style:\n"
assert_errors "$FLOW" status --pretty --strip-root --offset-style js-indices

printf "\nInvoking check with no offset-style argument:\n"
assert_errors "$FLOW" check --pretty --strip-root

printf "\nInvoking check with utf8-bytes offset-style:\n"
assert_errors "$FLOW" check --pretty --strip-root --offset-style utf8-bytes

printf "\nInvoking check with js-indices offset-style:\n"
assert_errors "$FLOW" check --pretty --strip-root --offset-style js-indices

printf "\nInvoking check-contents with no offset-style argument:\n"
# check-contents returns 0 even when errors are present
assert_ok "$FLOW" check-contents --pretty < foo.js

printf "\nInvoking check-contents with utf8-bytes offset-style:\n"
assert_ok "$FLOW" check-contents --pretty --offset-style utf8-bytes < foo.js

printf "\nInvoking check-contents with js-indices offset-style:\n"
assert_ok "$FLOW" check-contents --pretty --offset-style js-indices < foo.js

# Make sure there's a newline at the end of the file
echo
