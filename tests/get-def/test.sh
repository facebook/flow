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

printf "class properties = "
assert_ok "$FLOW" get-def class.js 9 6 --strip-root --pretty
printf "class methods = "
assert_ok "$FLOW" get-def class.js 10 6 --strip-root --pretty
printf "refined class properties = "
assert_ok "$FLOW" get-def class.js 13 8 --strip-root --pretty
