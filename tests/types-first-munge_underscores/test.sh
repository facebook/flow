#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nStop any already-running server."
assert_ok "$FLOW" stop .

printf "\nCheck with munge_underscores = false (by default) should return no errors:\n"
assert_ok "$FLOW" check --no-flowlib .

printf "\nCheck with --munge-underscore-members flag should return one error on the _x update:\n"
assert_errors "$FLOW" check --no-flowlib --munge-underscore-members .

# set 'munge_underscores = true' in .flowconfig
cp .flowconfig.munge_underscores_true .flowconfig

printf "\nCheck with munge_underscores=true in .flowconfig should return one error on the _x update:\n"
assert_errors "$FLOW" check --no-flowlib .

# prevent munge via pragma
sed -i'.orig' -e '1s/^/\/\/ @preventMunge\\n\n/' class.js

printf "\nCheck with preventMunge via pragma (overrides .flowconfig option) should return no errors:\n"
assert_ok "$FLOW" check --no-flowlib .
