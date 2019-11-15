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

printf "this2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this2.js 5 6 < this2.js

printf "typeparams.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty typeparams.js 6 16 < typeparams.js

printf "typeparams_function.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty typeparams_function.js 3 2 < typeparams_function.js

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
  "$FLOW" autocomplete --strip-root --pretty jsx2.js 7 9 < jsx2.js

printf "jsx3.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx3.js 9 4 < jsx3.js

printf "jsx4.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx4.js 7 3 < jsx4.js

printf "jsx-function-component.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-function-component.js 9 4 < jsx-function-component.js

printf "jsx-function-component-2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-function-component-2.js 9 11 < jsx-function-component-2.js

printf "jsx-function-component-3.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-function-component-3.js 10 4 < jsx-function-component-3.js

printf "jsx-abstract-component.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-abstract-component.js 8 4 < jsx-abstract-component.js

printf "jsx-with-children.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-with-children.js 10 4 < jsx-with-children.js

printf "jsx-text.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty jsx-text.js 1 11 < jsx-text.js

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

printf "member_class_property.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty member_class_property.js 8 5 < member_class_property.js

printf "member_class_static.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty member_class_static.js 7 5 < member_class_static.js

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

printf "identifier.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty identifier.js 10 18 < identifier.js

printf "super.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty super.js 4 8 < super.js

printf "this-2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this-2.js 5 10 < this-2.js
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this-2.js 8 3 < this-2.js
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this-2.js 11 15 < this-2.js
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty this-2.js 14 9 < this-2.js

printf "pattern.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty pattern.js 3 8 < pattern.js
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty pattern.js 6 4 < pattern.js
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty pattern.js 9 14 < pattern.js

printf "normalize-1.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty normalize-1.js 4 12 < normalize-1.js

printf "normalize-2.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty normalize-2.js 5 10 < normalize-2.js

printf "unqualified-type-annotation.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty unqualified-type-annotation.js 28 19 < unqualified-type-annotation.js

printf "qualified-type-annotation.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty qualified-type-annotation.js 5 23 < qualified-type-annotation.js

printf "qualified-type-annotation-require.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty qualified-type-annotation-require.js 5 23 < qualified-type-annotation-require.js

printf "inherited-class-properties.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty inherited-class-properties.js 16 13 < inherited-class-properties.js

printf "function-added-properties.js = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty function-added-properties.js 6 5 < function-added-properties.js

printf "comments.js (line) = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty comments.js 3 8 < comments.js

printf "comments.js (block) = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty comments.js 5 8 < comments.js

printf "literal.js (inside string) = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty literal.js 3 16 < literal.js

printf "literal.js (inside regex) = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty literal.js 5 16 < literal.js

printf "literal.js (inside template literal) = "
assert_ok \
  "$FLOW" autocomplete --strip-root --pretty literal.js 7 16 < literal.js
