#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# `THIS_DIR` is the flow root's `scripts/`, which a test tree built by Buck
# leaves out, so its parent has to be computed without resolving it.
website_docs="$(dirname "$THIS_DIR")/website/docs"

printf "unpack-docs docs\n"
assert_ok "$FLOW" agent-helper unpack-docs docs
diff -r "$website_docs" docs
printf "docs matches website/docs\n"

printf "\nunpack-docs docs (already unpacked)\n"
assert_ok "$FLOW" agent-helper unpack-docs docs
diff -r "$website_docs" docs
printf "docs matches website/docs\n"
