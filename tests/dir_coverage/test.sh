#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

backup_file() {
  cp a.js a.js.bak
}

restore_file() {
  cp a.js.bak a.js
}

coverage() {
  echo "-----------------------------"
  echo "restart server"
  echo "-----------------------------"
  assert_ok "$FLOW" stop
  assert_ok "$FLOW" start
  echo "-----------------------------"
  echo "root"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root .
  echo "-----------------------------"
  echo "folder"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root folder
  echo "-----------------------------"
  echo "cycle"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root cycle
  echo "-----------------------------"
  echo "match_coverage"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root match_coverage
  echo "-----------------------------"
  echo "other_folder"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root other_folder
  echo "-----------------------------"
  echo "folder/subfolder"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root folder/subfolder
  echo "-----------------------------"
  echo "file list"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root a.js folder/d.js folder/subfolder/j.js
  echo "-----------------------------"
  echo "file and dir list"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root a.js folder folder/d.js
  echo "-----------------------------"
  echo "files"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root --input-file files.txt
  echo "-----------------------------"
  echo "json"
  echo "-----------------------------"
  assert_ok "$FLOW" batch-coverage --strip-root --json --pretty --input-file files.txt
  echo "-----------------------------"
  echo "root info survives recheck"
  echo "-----------------------------"
  assert_ok cp a.js.ignored a.js
  assert_ok "$FLOW" force-recheck a.js
  assert_ok "$FLOW" batch-coverage --strip-root --wait-for-recheck true .
  echo "-----------------------------"
  echo "delete file"
  echo "-----------------------------"
  assert_ok rm a.js
  assert_ok "$FLOW" force-recheck a.js
  assert_ok "$FLOW" batch-coverage --strip-root --wait-for-recheck true .
  echo "-----------------------------"
  echo "trust"
  echo "-----------------------------"
  assert_ok "$FLOW" stop
  assert_ok "$FLOW" start --trust-mode=check
  assert_ok "$FLOW" batch-coverage --strip-root --show-trust .
  assert_ok "$FLOW" batch-coverage --strip-root --json --pretty --show-trust .
}

backup_file

# Run in classic
echo "============================="
echo "CLASSIC"
echo "============================="

coverage > classic-coverage.log

cat classic-coverage.log

restore_file

# Run in types-first
echo "============================="
echo "TYPES-FIRST"
echo "============================="
assert_ok mv .flowconfig.types-first .flowconfig
coverage > types-first-coverage.log

echo "==== DIFF BETWEEN CLASSIC AND TYPES-FIRST ====="
assert_ok diff --strip-trailing-cr classic-coverage.log types-first-coverage.log > diff.log
cat diff.log
