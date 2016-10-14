#!/bin/sh

FLOW=$1

"$FLOW" get-def example.js 12 10 --strip-root --json

# import thing from "./exports_default.js";
#           ^
printf "default import points to default export = "
"$FLOW" get-def imports.js 3 10 --strip-root --pretty

# thing;
#    ^
printf "local reference points to default import = "
"$FLOW" get-def imports.js 4 4 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#          ^
printf "named import points to named export = "
"$FLOW" get-def imports.js 6 10 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#                   ^
printf "aliased named import points to named export (as) = "
"$FLOW" get-def imports.js 6 19 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#               ^
printf "aliased named import points to named export (remote name) = "
"$FLOW" get-def imports.js 6 15 --strip-root --pretty

# import {foo, bar as baz} from "./helpers/exports_named.js";
#                      ^
printf "aliased named import points to named export (local name) = "
"$FLOW" get-def imports.js 6 22 --strip-root --pretty

# foo;
#  ^
printf "local reference points to named import = "
"$FLOW" get-def imports.js 7 2 --strip-root --pretty

# baz;
#  ^
printf "local reference points to aliased named import = "
"$FLOW" get-def imports.js 8 2 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#          ^
printf "namespaced import points to imported file (as) = "
"$FLOW" get-def imports.js 10 11 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#               ^
printf "namespaced import points to imported file (remote name) = "
"$FLOW" get-def imports.js 10 8 --strip-root --pretty

# import * as things from "./helpers/exports_named.js";
#        ^
printf "namespaced import points to imported file (local name) = "
"$FLOW" get-def imports.js 10 15 --strip-root --pretty

# things;
#    ^
printf "local reference points to namespaced import = "
"$FLOW" get-def imports.js 11 4 --strip-root --pretty
