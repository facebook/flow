#!/bin/bash
printf "foo_parse_fail.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root foo_parse_fail.js 10 17 < foo_parse_fail.js

printf "foo.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty foo.js 10 5 < foo.js

printf "bar.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty bar.js 4 5 < bar.js

printf "qux.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty qux.js 6 3 < qux.js

printf "str.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty str.js 3 9 < str.js

printf "num.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty num.js 4 5 < num.js

printf "bool.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty bool.js 3 6 < bool.js

printf "union.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty union.js 10 5 < union.js

printf "object_builtins.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty object_builtins.js 4 5 < object_builtins.js

printf "function_builtins.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty function_builtins.js 4 5 < function_builtins.js

printf "fun.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty fun.js 4 5 < fun.js

printf "this.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this.js 8 10 < this.js

printf "typeparams.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty typeparams.js 6 16 < typeparams.js

printf "generics.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty generics.js 6 5 < generics.js

printf "optional.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty optional.js 4 14 < optional.js

printf "jsx1.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx1.js 8 4 < jsx1.js

printf "jsx2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx2.js 8 11 < jsx2.js

printf "customfun.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty customfun.js 6 1 < customfun.js

printf "issue-1368.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty issue-1368.js 20 10 < issue-1368.js

printf "if.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty if.js 3 7 < if.js

printf "override.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty override.js 10 16 < override.js

printf "class.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty class.js 7 5 < class.js

printf "optional_chaining_new.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty optional_chaining_new.js 9 15 < optional_chaining_new.js

printf "optional_chaining_continue.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty optional_chaining_continue.js 13 19 < optional_chaining_continue.js

printf "idx.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty idx.js 12 28 < idx.js
