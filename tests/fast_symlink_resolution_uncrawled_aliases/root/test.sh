#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

if command -v cmd.exe > /dev/null 2>&1; then
  rm -f ignored_dir/sub/alias.js ../unwatched/alias.js
  cmd.exe /D /S /C 'mklink ignored_dir\sub\alias.js ..\..\util.js' > /dev/null
  cmd.exe /D /S /C 'mklink ..\unwatched\alias.js ..\root\util.js' > /dev/null
fi

printf "Scratch init:\n"
assert_errors "$FLOW" status .

printf "\nRecheck after scratch init:\n"
printf "\n// recheck after scratch init\n" >> foo.js
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

assert_ok "$FLOW" save-state --root . --out .flow.saved_state > /dev/null
assert_ok "$FLOW" stop .
: > .flow.saved_state_file_changes

printf "\nSaved-state init:\n"
start_flow . --saved-state-fetcher local --saved-state-no-fallback
assert_errors "$FLOW" status --no-auto-start .

printf "\nRecheck after saved-state init:\n"
printf "\n// recheck after saved-state init\n" >> foo.js
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

assert_ok "$FLOW" stop .
