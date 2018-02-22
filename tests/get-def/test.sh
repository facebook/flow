#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" get-def example.js 12 10 --strip-root --json

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
printf "named import points to named export = "
assert_ok "$FLOW" get-def imports.js 6 10 --strip-root --pretty

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
