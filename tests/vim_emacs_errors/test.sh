#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "from emacs:\n"
assert_errors "$FLOW" status --strip-root --from emacs

printf "from vim:\n"
assert_errors "$FLOW" status --strip-root --from vim
