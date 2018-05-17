#!/bin/bash
. ../assert.sh
FLOW=$1

echo "simple ES6 exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 3 17

echo "ES6 default export:"
# export default class Bar {}
#           ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 7 11

echo "ES6 default export from the identifier:"
# export default class Bar {}
#                      ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 7 22

echo "ES6 default export from a use in the file where it is defined:"
# new Bar();
#     ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 25 5

echo "ES6 default import:"
# import Bar, {foo, Foo} from './es6-1';
#        ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 8

echo "Use of ES6 default import:"
# new Bar();
#      ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 19 6

echo "Use of ES6 default import imported through import *"
# new all.default();
#          ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 20 10

echo

echo "shadowing an export:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 12 15

echo

echo "class method:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 17 3

echo "class method, queried from a use instead of the definition:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 17 4

echo "class method that overrides a superclass implementation:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root extendsFoo.js 7 3

echo "local exported as CJS property:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 2 7

echo "CJS property exporting local:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 3 16

echo "CJS property exporting literal:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 4 16

echo "local exported in CJS object:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 2 7

echo "CJS object exporting local:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 19

echo "CJS object exporting literal:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 27

echo "CJS object exporting shorthand:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 37

echo "CJS ident exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-3.js 2 7
