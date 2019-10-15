#!/bin/bash
# shellcheck disable=SC2094

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
  "$FLOW" autocomplete --strip-root --pretty jsx1.js 7 4 < jsx1.js

printf "jsx2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx2.js 7 11 < jsx2.js

printf "jsx3.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx3.js 9 4 < jsx3.js

printf "jsx-function-component.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-function-component.js 9 4 < jsx-function-component.js

printf "jsx-abstract-component.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-abstract-component.js 8 4 < jsx-abstract-component.js

printf "jsx-with-children.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-with-children.js 10 4 < jsx-with-children.js

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

printf "generic_alias.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty generic_alias.js 7 5 < generic_alias.js

printf "object_literal.js:5:16 = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty object_literal.js 5 16 < object_literal.js

printf "object_literal.js:7:17 = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty object_literal.js 7 17 < object_literal.js

printf "optional_object.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty optional_object.js 3 5 < optional_object.js
  
printf "indirect_array.js:5:3 = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty indirect_array.js 5 3 < indirect_array.js

printf "indirect_array.js:10:3 = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty indirect_array.js 10 3 < indirect_array.js

printf "infer.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty infer.js 4 5 < infer.js

printf "eval_predicate.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty eval_predicate.js 5 3 < eval_predicate.js

printf "eval_destructor.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty eval_destructor.js 5 3 < eval_destructor.js

printf "poly.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty poly.js 5 3 < poly.js

printf "poly_no_args.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty poly_no_args.js 5 3 < poly_no_args.js
