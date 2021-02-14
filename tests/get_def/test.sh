#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" get-def example.js 12 10 --strip-root --json

# foo;
#  ^
printf "use of const binding = "
assert_ok "$FLOW" get-def const.js 7 4 --strip-root --pretty
printf "use of shadowed const binding = "
assert_ok "$FLOW" get-def const.js 10 2 --strip-root --pretty

printf "let binding = "
assert_ok "$FLOW" get-def let.js 3 6 --strip-root --pretty
printf "use of let binding = "
assert_ok "$FLOW" get-def let.js 7 4 --strip-root --pretty
printf "use of shadowed let binding = "
assert_ok "$FLOW" get-def let.js 10 2 --strip-root --pretty
printf "uninitialized let binding = "
assert_ok "$FLOW" get-def let.js 12 6 --strip-root --pretty
printf "conditional write of let binding = "
assert_ok "$FLOW" get-def let.js 14 4 --strip-root --pretty
printf "use of conditionally initialized let binding = "
assert_ok "$FLOW" get-def let.js 18 2 --strip-root --pretty
printf "let binding initialized to member expression = "
assert_ok "$FLOW" get-def let.js 28 8 --strip-root --pretty

# import thing from "./exports_default.js";
#           ^
printf "default import points to default export = "
assert_ok "$FLOW" get-def imports.js 3 10 --strip-root --pretty

# thing;
#    ^
printf "local reference points to default import = "
assert_ok "$FLOW" get-def imports.js 4 4 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#          ^
printf "named import of const points to named export = "
assert_ok "$FLOW" get-def imports.js 6 10 --strip-root --pretty

printf "named import of let points to named export = "
assert_ok "$FLOW" get-def imports.js 6 29 --strip-root --pretty

printf "named import of var points to named export = "
assert_ok "$FLOW" get-def imports.js 6 40 --strip-root --pretty

printf "named import of function points to named export = "
assert_ok "$FLOW" get-def imports.js 6 51 --strip-root --pretty

printf "named import of class points to named export = "
assert_ok "$FLOW" get-def imports.js 6 56 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#                   ^
printf "aliased named import points to named export (as) = "
assert_ok "$FLOW" get-def imports.js 6 19 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#               ^
printf "aliased named import points to named export (remote name) = "
assert_ok "$FLOW" get-def imports.js 6 15 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#                      ^
printf "aliased named import points to named export (local name) = "
assert_ok "$FLOW" get-def imports.js 6 22 --strip-root --pretty

# foo;
#  ^
printf "local reference points to named import = "
assert_ok "$FLOW" get-def imports.js 7 2 --strip-root --pretty

# baz;
#  ^
printf "local reference points to aliased named import = "
assert_ok "$FLOW" get-def imports.js 8 2 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#          ^
printf "namespaced import points to imported file (as) = "
assert_ok "$FLOW" get-def imports.js 10 11 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#               ^
printf "namespaced import points to imported file (remote name) = "
assert_ok "$FLOW" get-def imports.js 10 8 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#        ^
printf "namespaced import points to imported file (local name) = "
assert_ok "$FLOW" get-def imports.js 10 15 --strip-root --pretty

# things;
#    ^
printf "local reference points to namespaced import = "
assert_ok "$FLOW" get-def imports.js 11 4 --strip-root --pretty

printf "class name (should be itself) = "
assert_ok "$FLOW" get-def class.js 3 8 --strip-root --pretty
printf "class property read = "
assert_ok "$FLOW" get-def class.js 13 6 --strip-root --pretty
printf "class property write = "
assert_ok "$FLOW" get-def class.js 14 6 --strip-root --pretty
printf "class methods = "
assert_ok "$FLOW" get-def class.js 15 6 --strip-root --pretty
printf "refined class properties = "
assert_ok "$FLOW" get-def class.js 18 8 --strip-root --pretty
# TODO get private properties working
printf "private class property access = "
assert_ok "$FLOW" get-def class.js 7 15 --strip-root --pretty
printf "private class property assignment = "
assert_ok "$FLOW" get-def class.js 8 15 --strip-root --pretty
printf "members of maybe types = "
assert_ok "$FLOW" get-def class.js 22 5 --strip-root --pretty
printf "members of unions with null/void = "
assert_ok "$FLOW" get-def class.js 26 5 --strip-root --pretty

printf "member of a nonexistent imported type with type parameters = "
assert_ok "$FLOW" get-def imports.js 16 4 --strip-root --pretty 2>&1
printf "member of a type alias for \`any\` with type parameters = "
assert_ok "$FLOW" get-def imports.js 22 4 --strip-root --pretty 2>&1

printf "member of an object type alias = "
assert_ok "$FLOW" get-def objects.js 5 4 --strip-root --pretty
printf "member of an unannotated object type = "
assert_ok "$FLOW" get-def objects.js 8 4 --strip-root --pretty
printf "shadow prop created on write = "
assert_ok "$FLOW" get-def objects.js 12 4 --strip-root --pretty
printf "shadow prop created on read = "
assert_ok "$FLOW" get-def objects.js 14 4 --strip-root --pretty

printf "optional chain initial property = "
assert_ok "$FLOW" get-def optional_chaining.js 17 6 --strip-root --pretty
printf "optional chain subsequent property = "
assert_ok "$FLOW" get-def optional_chaining.js 17 10 --strip-root --pretty
printf "optional chain initial property of null = "
assert_ok "$FLOW" get-def optional_chaining.js 18 7 --strip-root --pretty
printf "optional chain subsequent property of null = "
assert_ok "$FLOW" get-def optional_chaining.js 18 11 --strip-root --pretty

printf "shorthand destructuring = "
assert_ok "$FLOW" get-def objects.js 19 11 --strip-root --pretty
printf "non-shorthand destructuring = "
assert_ok "$FLOW" get-def objects.js 20 11 --strip-root --pretty
printf "destructuring without type alias = "
assert_ok "$FLOW" get-def objects.js 22 11 --strip-root --pretty
printf "destructuring a shadow prop = "
assert_ok "$FLOW" get-def objects.js 23 11 --strip-root --pretty
# This one should return itself
printf "bogus array destructuring of an object = "
assert_ok "$FLOW" get-def objects.js 24 11 --strip-root --pretty
printf "property access assigned to a variable = "
assert_ok "$FLOW" get-def objects.js 25 10 --strip-root --pretty

printf "property access on the arg to the idx callback = "
assert_ok "$FLOW" get-def idx.js 12 25 --strip-root --pretty
printf "nested property access on the arg to the idx callback = "
assert_ok "$FLOW" get-def idx.js 12 29 --strip-root --pretty

printf "global (builtin) variable = "
assert_ok "$FLOW" get-def global.js 3 3 --strip-root --pretty

printf "object destructuring key = "
assert_ok "$FLOW" get-def destructuring.js 5 9 --strip-root --pretty
printf "object destructuring binding = "
assert_ok "$FLOW" get-def destructuring.js 5 14 --strip-root --pretty

queries_in_file "get-def" "annot.js"
queries_in_file "get-def" "exports.js"
queries_in_file "get-def" "identifier.js"
