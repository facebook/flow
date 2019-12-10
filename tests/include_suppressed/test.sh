#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "Without --include-suppressed\n"
assert_errors "$FLOW" check --all --strip-root

printf "\n\nWith --include-suppressed\n"
assert_errors "$FLOW" check --all --strip-root --include-suppressed

printf "\n\nJSON without --include-suppressed\n"
assert_errors \
  "$FLOW" check --all --strip-root --json --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n\nJSON with --include-suppressed\n"
assert_errors \
  "$FLOW" check --all --strip-root --include-suppressed --json --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n\nServer without --include-suppressed\n"
"$FLOW" start --wait --all
assert_errors "$FLOW" status --no-auto-start
"$FLOW" stop

printf "\n\nServer with --include-suppressed\n"
"$FLOW" start --wait --all --include-suppressed
assert_errors "$FLOW" status --no-auto-start
"$FLOW" stop

printf "\n"
