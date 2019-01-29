#!/bin/bash


echo "flow check --json-version=2"
assert_errors "$FLOW" check . --strip-root --json-version=2
echo ""
echo ""

echo "flow check --json-version=2 --json"
assert_errors "$FLOW" check . --strip-root --json-version=2 --json
echo ""
echo ""

echo "flow check --json-version=2 --pretty"
assert_errors "$FLOW" check . --strip-root --json-version=2 --pretty
echo ""
echo ""

echo "flow focus-check test.js --json-version=2"
assert_errors "$FLOW" focus-check test.js --strip-root --json-version=2
echo ""
echo ""

echo "flow focus-check test.js --json-version=2 --json"
assert_errors "$FLOW" focus-check test.js --strip-root --json-version=2 --json
echo ""
echo ""

echo "flow focus-check test.js --json-version=2 --pretty"
assert_errors "$FLOW" focus-check test.js --strip-root --json-version=2 --pretty
echo ""
echo ""

echo "flow check-contents --json-version=2 < test.js"
assert_ok "$FLOW" check-contents --json-version=2 < test.js
echo ""
echo ""

echo "flow check-contents --json-version=2 --json < test.js"
assert_ok "$FLOW" check-contents --json-version=2 --json < test.js
echo ""
echo ""

echo "flow check-contents --json-version=2 --pretty < test.js"
assert_ok "$FLOW" check-contents --json-version=2 --pretty < test.js
echo ""
echo ""

echo "flow status --json-version=2"
assert_errors "$FLOW" status . --strip-root --json-version=2
echo ""
echo ""

echo "flow status --json-version=2 --json"
assert_errors "$FLOW" status . --strip-root --json-version=2 --json
echo ""
echo ""

echo "flow status --json-version=2 --pretty"
assert_errors "$FLOW" status . --strip-root --json-version=2 --pretty
echo ""
