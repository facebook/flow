#!/bin/bash
echo "ES6 named export:"
# export function foo() {
#                 ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 3 17

echo "Local use of an ES6 named export:"
# foo();
# ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 9 1

echo "ES6 named import:"
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
#              ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 14

echo "Use of ES6 named import:"
# foo();
# ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 6 1

echo "Use of ES6 named import through import *:"
# all.foo();
#     ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 22 5

echo "Local name of an aliased ES6 named import:"
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
#                                  ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 34

echo "Remote name of an aliased ES6 named import:"
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
#                        ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 24

echo "Local use of an aliased ES6 named import:"
# localBaz;
#      ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 24 6

echo "Second local name of an aliased ES6 named import:"
# For some reason this is allowed
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
#                                                  ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 50

echo "Second remote name of an aliased ES6 named import:"
# For some reason this is allowed
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
#                                          ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 3 42

echo "Local use of the second name of an aliased ES6 named import:"
# otherBaz;
#      ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 25 6

echo "Named ES6 export created as part of a single declaration with multiple names:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 27 24

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
# import Bar, {foo, Foo, baz as localBaz, baz as otherBaz} from './es6-1';
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

echo "ES6 default export of an async function:"
# export default async function foo() { foo(); }
#                                ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root exportDefaultAsync.js 3 32

echo "ES6 import of default async function:"
# import fooAsync from './exportDefaultAsync';
#          ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root importDefault.js 3 10

echo "ES6 default export of an arbitrary expression:"
# export default (1, function foo() { foo(); });
#         ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root exportDefaultExpr.js 5 9

echo "Function expression exported via export default:"
# export default (1, function foo() { foo(); });
#                              ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root exportDefaultExpr.js 5 30

echo "Function declaration is not shadowed by export default expression:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root exportDefaultExpr.js 9 2

echo

echo "ES6 import of default expression:"
# import foo from './exportDefaultExpr';
#          ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root importDefault.js 4 10

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
# const a = 5;
#       ^
# module.exports.foo = a;
#
# We expect this to return only references to the local. Users can find-refs on `foo` for global
# results.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 2 7

echo "CJS property exporting local:"
# module.exports.foo = a;
#                ^
#
# This should return downstream refrences to `foo`, plus related locals in the special case where
# the result of a `require` is immediately destructured.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 3 16

echo "CJS property exporting literal:"
# module.exports.foo2 = 0;
#                ^
#
# This should behave the same as the previous case since we no longer associate locals with their
# exports.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-1.js 4 16

echo "local exported in CJS object:"
# const b = 4;
#       ^
# module.exports = {bar: b, bar2: 42, bar3};
#
# This should return only the local uses of `b`.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 2 7

echo "CJS object exporting local:"
# module.exports = {bar: b, bar2: 42, bar3};
#                   ^
#
# This should return downstream references to the exported value `bar` as well as their associated
# locals in some special cases.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 19

echo "CJS object exporting literal:"
# Same as above
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 27

echo "CJS object exporting shorthand:"
# Same as above
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-2.js 4 37

echo "CJS ident exports/imports:"
# const baz = {c: 0};
#       ^
# module.exports = baz;
#
# Should include references only to the local, since we no longer want to associate local variables
# with their exports.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-3.js 2 7

echo "CJS default exports:"
# module.exports = baz;
#           ^
# Should include downstream `require` calls which evaluate to this module, as well as the locals
# that they are bound to in some specific cases.
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-3.js 3 11

echo "CJS default imports:"
# const baz = require('./cjs-3');
#             ^
# Should have the same results as above
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-4.js 4 13

echo "CJS default imports bound to a local:"
# const baz = require('./cjs-3');
#       ^
# Should have the same results as above
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root cjs-4.js 4 7

echo "declare var:"
# declare var foo: number;
#              ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root declare.js 3 14

echo "declare export var:"
# declare export var bar;
#                     ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root declare.js 6 21

echo "declare function:"
# declare function baz(): void;
#                   ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root declare.js 9 19

echo "declare class:"
# declare class Foo {};
#                ^
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root declare.js 12 16
