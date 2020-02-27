#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test expected output
echo "Test: flow-remove-types test/source.js"
DIFF=$(./flow-remove-types test/source.js | diff test/expected.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test expected output with --ignore-uninitialized-fields flag
echo "Test: flow-remove-types --ignore-uninitialized-fields test/source.js"
DIFF=$(./flow-remove-types --ignore-uninitialized-fields test/source.js | diff test/expected-uninitialized-fields.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test expected output with --pretty flag
echo "Test: flow-remove-types --pretty test/source.js"
DIFF=$(./flow-remove-types --pretty test/source.js | diff test/expected-pretty.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test expected source maps with --pretty --sourcemaps
echo "Test: flow-remove-types --pretty --sourcemaps test/source.js -d test/expected-with-maps"
TEST_DIR=$(
  DIR=$(dirname "${BASH_SOURCE[0]}");
  cd "$DIR" && pwd
)
DIR=$(mktemp -d)
cp -r test "$DIR"
pushd "$DIR" || exit 1 > /dev/null
"$TEST_DIR/flow-remove-types" --pretty --sourcemaps test/source.js -d test/expected-with-maps;
popd || exit 1 > /dev/null
DIFF_SOURCE=$(diff test/expected-with-maps/test/source.js "$DIR/test/expected-with-maps/test/source.js");
DIFF_MAP=$(diff test/expected-with-maps/test/source.js.map "$DIR/test/expected-with-maps/test/source.js.map");
rm -rf "$DIR"
if [ -n "$DIFF_SOURCE" ]; then echo "$DIFF_SOURCE"; exit 1; fi;
if [ -n "$DIFF_MAP" ]; then echo "$DIFF_MAP"; exit 1; fi;

# Test expected source maps with --pretty --sourcemaps inline
echo "Test: flow-remove-types --pretty --sourcemaps inline test/source.js"
DIFF=$(./flow-remove-types --pretty --sourcemaps inline test/source.js | diff test/expected-pretty-inlinemap.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test expected source maps with --pretty --sourcemaps inline, from stdin
echo "Test: flow-remove-types --pretty --sourcemaps inline < test/source.js"
DIFF=$(./flow-remove-types --pretty --sourcemaps inline < test/source.js | diff test/expected-pretty-inlinemap-stdin.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test expected output with @flow outside of comments
echo "Test: flow-remove-types test/without-flow.js"
DIFF=$(./flow-remove-types test/without-flow.js | diff test/without-flow.js -);
if [ -n "$DIFF" ]; then echo "$DIFF"; exit 1; fi;

# Test node require hook
echo "Test: node require hook"
RES=$(node -e 'require("./register");require("./test/test-node-module.js")');
if [ "$RES" != 42 ]; then echo 'Node require hook failed'; exit 1; fi;

# Test flow-node
echo "Test: flow-node"
FLOW_NODE=$(./flow-node ./test/test-node-module.js);
if [ "$FLOW_NODE" != 42 ]; then echo 'flow-node failed'; exit 1; fi;

# Test flow-node with options
echo "Test: flow-node with options"
FLOW_NODE_OPTS=$(./flow-node --code-comments -p 'process.argv.length');
if [ "$FLOW_NODE_OPTS" != 4 ]; then echo 'flow-node with options failed'; exit 1; fi;
