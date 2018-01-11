#!/bin/bash
. ../assert.sh
FLOW=$1

printf "foo_parse_fail.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js

printf "foo.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json foo.js 10 5 < foo.js

printf "bar.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json bar.js 4 5 < bar.js

printf "qux.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json qux.js 6 3 < qux.js

printf "str.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json str.js 3 9 < str.js

printf "num.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json num.js 4 5 < num.js

printf "bool.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json bool.js 3 6 < bool.js

printf "union.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json union.js 10 5 < union.js

printf "object_builtins.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json object_builtins.js 4 5 < object_builtins.js

printf "function_builtins.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json function_builtins.js 4 5 < function_builtins.js

printf "fun.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json fun.js 4 5 < fun.js

printf "this.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json this.js 8 10 < this.js

printf "typeparams.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json typeparams.js 6 16 < typeparams.js

printf "generics.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json generics.js 6 5 < generics.js

printf "optional.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json optional.js 4 14 < optional.js

printf "jsx1.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json jsx1.js 8 4 < jsx1.js

printf "jsx2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json jsx2.js 8 11 < jsx2.js

printf "customfun.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json customfun.js 11 2 < customfun.js

printf "issue-1368.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json issue-1368.js 20 10 < issue-1368.js

printf "if.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json if.js 3 7 < if.js

printf "override.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json override.js 10 16 < override.js

printf "class.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --json class.js 7 5 < class.js
