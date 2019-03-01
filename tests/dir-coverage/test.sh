#!/bin/bash
echo "-----------------------------"
echo "restart server"
echo "-----------------------------"
echo
assert_ok "$FLOW" stop
assert_ok "$FLOW" start
echo "-----------------------------"
echo "root"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root .
echo "-----------------------------"
echo "folder"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root folder
echo "-----------------------------"
echo "cycle"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root cycle
echo "-----------------------------"
echo "match_coverage"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root match_coverage
echo "-----------------------------"
echo "other_folder"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root other_folder
echo "-----------------------------"
echo "folder/subfolder"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root folder/subfolder
echo "-----------------------------"
echo "file list"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root a.js folder/d.js folder/subfolder/j.js
echo "-----------------------------"
echo "file and dir list"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root a.js folder folder/d.js
echo "-----------------------------"
echo "files"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root --input-file files.txt
echo "-----------------------------"
echo "json"
echo "-----------------------------"
echo
assert_ok "$FLOW" batch-coverage --strip-root --json --pretty --input-file files.txt
