#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# All these functions assume the $FLOW env variable is set to the Flow binary.
# Globs should be expanded when you call these functions, so things like
#
#     remove *.js
#
# should work.

remove() {
  set -e
  # shellcheck disable=SC2068
  rm $@;
  # shellcheck disable=SC2068
  assert_ok "$FLOW" force-recheck $@;
}

# This only supports the mv a.js b.js form. It doesn't support mv *.js dir/
move() {
  set -e
  # shellcheck disable=SC2068
  mv $@;
  # shellcheck disable=SC2068
  assert_ok "$FLOW" force-recheck $@;
}

# This only supports the cp a.js b.js form. It doesn't support cp *.js dir/
copy() {
    set -e
    # shellcheck disable=SC2068
    cp $@;
    # shellcheck disable=SC2068
    assert_ok "$FLOW" force-recheck $@;
}
