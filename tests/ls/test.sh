#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# don't be smart with sorting to be consistent across platforms
export LC_ALL=C

echo "================================================================================"
echo "No --all flag and implicit root"
echo "================================================================================"

echo "1. Assumes current directory is root, and there's no .flowconfig"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" ls 2>&1
echo ""

echo "2. Infers root and only shows included files in src directory"
assert_ok "$FLOW" ls --strip-root src | sort
echo ""

echo "3. Infers root and will show included files in both directories"
assert_ok "$FLOW" ls --strip-root src other | sort
echo ""

echo "4. Infers root from first arg, which is not a flow root"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" ls 2>&1
echo ""

echo "5. Won't show files that don't exist"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" ls --strip-root src/doesNotExist.js
echo ""

echo "================================================================================"
echo "Explicit root will not filter out files in other/"
echo "================================================================================"
assert_ok "$FLOW" ls --strip-root --root src | sort
echo ""

echo "================================================================================"
echo "--all should all libs, included files, and explicitly ignored files"
echo "================================================================================"
assert_ok "$FLOW" ls --strip-root --all --root src | sort
echo ""

echo "================================================================================"
echo "Implicit/Explicit Included/Ignored/Lib should be correct"
echo "================================================================================"
assert_ok "$FLOW" ls --strip-root --root src --all --explain $(cat files.txt) | sort
echo ""

echo "================================================================================"
echo "JSON output without --explain should be an array"
echo "================================================================================"
assert_ok "$FLOW" ls --json --strip-root --root src --all $(cat files.txt) src/.flowconfig
echo ""

echo "================================================================================"
echo "JSON output with --explain should be JSON object"
echo "================================================================================"
assert_ok "$FLOW" ls --json --strip-root --root src --all --explain $(cat files.txt) src/.flowconfig
echo ""

echo "================================================================================"
echo "Listing files over stdin"
echo "================================================================================"

echo "1. Same as if we passed src/ and other/explicitly_include.js from the command line'"
assert_ok "$FLOW" ls --strip-root --root src --all --input-file '-' < stdin_file.txt | sort
echo ""

echo "2. flow ls will combine command line with the input file"
assert_ok "$FLOW" ls --strip-root \
  --root src --all --input-file '-' other/implicitly_ignored.js < stdin_file.txt | sort
echo ""

echo "================================================================================"
echo "Input file on disk"
echo "================================================================================"

echo "1. Same as if we passed src/ and other/explicitly_include.js from the command line'"
assert_ok "$FLOW" ls --strip-root --root src --all --input-file stdin_file.txt | sort
echo ""

echo "2. flow ls will combine command line with the input file"
assert_ok "$FLOW" ls --strip-root \
  --root src --all \--input-file stdin_file.txt other/implicitly_ignored.js | sort
echo ""

echo "================================================================================"
echo "Non-existent files and directories"
echo "================================================================================"

echo "1. We try to use foobar to infer the root, so we complain when it doesnt exist"
assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" "$FLOW" ls --strip-root src/foobar 2>&1
echo ""

echo "2. We just filter out non-existent files"
assert_ok "$FLOW" ls --strip-root --root src src/foobar src/implicitly_included.js
echo ""

echo "3. With --imaginary we include non-existent files. Non-existent files are never considered to be libs."
assert_ok "$FLOW" ls --strip-root \
  --imaginary --root src src/foobar src/implicitly_included.js src/flow-typed/baz.js
echo ""

echo "4. --explain should work with --imaginary as expected. Non-existent files are never considered to be libs."
assert_ok "$FLOW" ls --strip-root --explain --imaginary --root \
  src src/foobar src/baz src/implicitly_included.js src/flow-typed/baz
echo ""

echo "5. We just filter out non-existent files. --all does not imply --imaginary"
assert_ok "$FLOW" ls --strip-root --all --root src src/foobar src/implicitly_included.js
echo ""

echo "6. ../foobar is implicitly ignored and only listed with the --all flag"
assert_ok "$FLOW" ls --strip-root --explain --all --imaginary --root src foobar src/foobar src/implicitly_included.js
