#!/bin/bash

# any.js
printf "any.js:3:15 = "
assert_ok "$FLOW" type-at-pos any.js 3 15 --strip-root --pretty
printf "any.js:4:2 = "
assert_ok "$FLOW" type-at-pos any.js 4 2 --strip-root --pretty
printf "any.js:4:6 = "
assert_ok "$FLOW" type-at-pos any.js 4 6 --strip-root --pretty
printf "any.js:5:5 = "
assert_ok "$FLOW" type-at-pos any.js 5 5 --strip-root --pretty
printf "any.js:7:13 = "
assert_ok "$FLOW" type-at-pos any.js 7 13 --strip-root --pretty
printf "any.js:8:5 = "
assert_ok "$FLOW" type-at-pos any.js 8 5 --strip-root --pretty
printf "any.js:8:10 = "
assert_ok "$FLOW" type-at-pos any.js 8 10 --strip-root --pretty
printf "any.js:9:10 = "
assert_ok "$FLOW" type-at-pos any.js 9 10 --strip-root --pretty

# array.js
# TODO `Array` is not populated in type_tables
# printf "array.js:3:18 = "
# assert_ok "$FLOW" type-at-pos array.js 3 18 --strip-root --pretty
# TODO `$ReadOnlyArray` is not populated in type_tables
# printf "array.js:4:30 = "
# assert_ok "$FLOW" type-at-pos array.js 4 30 --strip-root --pretty
printf "array.js:6:15 = "
assert_ok "$FLOW" type-at-pos array.js 6 15 --strip-root --pretty
printf "array.js:10:15 = "
assert_ok "$FLOW" type-at-pos array.js 10 15 --strip-root --pretty
printf "array.js:15:4 = "
assert_ok "$FLOW" type-at-pos array.js 15 4 --strip-root --pretty
printf "array.js:19:4 = "
assert_ok "$FLOW" type-at-pos array.js 19 4 --strip-root --pretty
printf "array.js:23:4 = "
assert_ok "$FLOW" type-at-pos array.js 23 4 --strip-root --pretty

# destructuring.js
printf "destructuring.js:3:6 = "
assert_ok "$FLOW" type-at-pos destructuring.js 3 6 --strip-root --pretty
printf "destructuring.js:17:13 = "
assert_ok "$FLOW" type-at-pos destructuring.js 17 13 --strip-root --pretty

# exact.js
printf "exact.js:4:6 = "
assert_ok "$FLOW" type-at-pos exact.js 4 6 --strip-root --pretty
printf "exact.js:5:13 = "
assert_ok "$FLOW" type-at-pos exact.js 5 13 --strip-root --pretty
printf "exact.js:6:13 = "
assert_ok "$FLOW" type-at-pos exact.js 6 13 --strip-root --pretty
printf "exact.js:7:13 = "
assert_ok "$FLOW" type-at-pos exact.js 7 13 --strip-root --pretty
printf "exact.js:9:17 = "
assert_ok "$FLOW" type-at-pos exact.js 9 17 --strip-root --pretty
printf "exact.js:10:7 = "
assert_ok "$FLOW" type-at-pos exact.js 10 7 --strip-root --pretty
printf "exact.js:13:13 = "
assert_ok "$FLOW" type-at-pos exact.js 13 13 --strip-root --pretty
printf "exact.js:16:13 = "
assert_ok "$FLOW" type-at-pos exact.js 16 13 --strip-root --pretty
printf "exact.js:18:6 = "
assert_ok "$FLOW" type-at-pos exact.js 18 6 --strip-root --pretty
printf "exact.js:19:6 = "
assert_ok "$FLOW" type-at-pos exact.js 19 6 --strip-root --pretty

# generics.js
printf "generics.js:5:1 = "
assert_ok "$FLOW" type-at-pos generics.js 5 1 --strip-root --pretty
printf "generics.js:10:1 = "
assert_ok "$FLOW" type-at-pos generics.js 10 1 --strip-root --pretty
printf "generics.js:14:1 = "
assert_ok "$FLOW" type-at-pos generics.js 14 1 --strip-root --pretty
printf "generics.js:18:1 = "
assert_ok "$FLOW" type-at-pos generics.js 18 1 --strip-root --pretty
printf "generics.js:22:1 = "
assert_ok "$FLOW" type-at-pos generics.js 22 1 --strip-root --pretty
printf "generics.js:30:13 = "
assert_ok "$FLOW" type-at-pos generics.js 30 13 --strip-root --pretty

# implicit-instantiation.js
printf "implicit-instantiation.js:5:10"
assert_ok "$FLOW" type-at-pos implicit-instantiation.js 5 10 --strip-root --pretty --expand-json-output
printf "implicit-instantiation.js:6:10"
assert_ok "$FLOW" type-at-pos implicit-instantiation.js 6 10 --strip-root --pretty --expand-json-output
printf "implicit-instantiation.js:10:21"
assert_ok "$FLOW" type-at-pos implicit-instantiation.js 10 21 --strip-root --pretty --expand-json-output

# interface.js
printf "interface.js:6:7 = "
assert_ok "$FLOW" type-at-pos interface.js 6 7 --strip-root
printf "interface.js:7:7 = "
assert_ok "$FLOW" type-at-pos interface.js 7 7 --strip-root
printf "interface.js:8:7 = "
assert_ok "$FLOW" type-at-pos interface.js 8 7 --strip-root
printf "interface.js:9:7 = "
assert_ok "$FLOW" type-at-pos interface.js 9 7 --strip-root
printf "interface.js:10:7 = "
assert_ok "$FLOW" type-at-pos interface.js 10 7 --strip-root
printf "interface.js:11:7 = "
assert_ok "$FLOW" type-at-pos interface.js 11 7 --strip-root
printf "interface.js:12:7 = "
assert_ok "$FLOW" type-at-pos interface.js 12 7 --strip-root

# mixed.js
printf "mixed.js:18:17 = "
assert_ok "$FLOW" type-at-pos mixed.js 18 17 --strip-root --pretty

# callable-object.js
printf "callable-object.js:8:6 = "
assert_ok "$FLOW" type-at-pos callable-object.js 8 6 --strip-root --pretty
printf "callable-object.js:13:6 = "
assert_ok "$FLOW" type-at-pos callable-object.js 13 6 --strip-root --pretty
printf "callable-object.js:18:6 = "
assert_ok "$FLOW" type-at-pos callable-object.js 18 6 --strip-root --pretty
printf "callable-object.js:23:6 = "
assert_ok "$FLOW" type-at-pos callable-object.js 23 6 --strip-root --pretty
printf "callable-object.js:31:6 = "
assert_ok "$FLOW" type-at-pos callable-object.js 31 6 --strip-root --pretty

# opaque.js
printf "opaque.js:3:20 = "
assert_ok "$FLOW" type-at-pos opaque.js 3 20 --strip-root --pretty
printf "opaque.js:4:14 = "
assert_ok "$FLOW" type-at-pos opaque.js 4 14 --strip-root --pretty
printf "opaque.js:4:19 = "
assert_ok "$FLOW" type-at-pos opaque.js 4 19 --strip-root --pretty
printf "opaque.js:6:22 = "
assert_ok "$FLOW" type-at-pos opaque.js 6 22 --strip-root --pretty
printf "opaque.js:7:13 = "
assert_ok "$FLOW" type-at-pos opaque.js 7 13 --strip-root --pretty
printf "opaque.js:7:18 = "
assert_ok "$FLOW" type-at-pos opaque.js 7 18 --strip-root --pretty
printf "opaque.js:9:22 = "
assert_ok "$FLOW" type-at-pos opaque.js 9 22 --strip-root --pretty
printf "opaque.js:10:13 = "
assert_ok "$FLOW" type-at-pos opaque.js 10 13 --strip-root --pretty
printf "opaque.js:10:18 = "
assert_ok "$FLOW" type-at-pos opaque.js 10 18 --strip-root --pretty
printf "opaque.js:12:14 = "
assert_ok "$FLOW" type-at-pos opaque.js 12 14 --strip-root --pretty
printf "opaque.js:13:14 = "
assert_ok "$FLOW" type-at-pos opaque.js 13 14 --strip-root --pretty
printf "opaque.js:13:19 = "
assert_ok "$FLOW" type-at-pos opaque.js 13 19 --strip-root --pretty
printf "opaque.js:15:22 = "
assert_ok "$FLOW" type-at-pos opaque.js 15 22 --strip-root --pretty
printf "opaque.js:16:14 = "
assert_ok "$FLOW" type-at-pos opaque.js 16 14 --strip-root --pretty
printf "opaque.js:16:19 = "
assert_ok "$FLOW" type-at-pos opaque.js 16 19 --strip-root --pretty
printf "opaque.js:19:14 = "
assert_ok "$FLOW" type-at-pos opaque.js 19 14 --strip-root --pretty
printf "opaque.js:19:22 = "
assert_ok "$FLOW" type-at-pos opaque.js 19 22 --strip-root --pretty
printf "opaque.js:20:16 = "
assert_ok "$FLOW" type-at-pos opaque.js 20 16 --strip-root --pretty
printf "opaque.js:20:34 = "
assert_ok "$FLOW" type-at-pos opaque.js 20 34 --strip-root --pretty
printf "opaque.js:21:19 = "
assert_ok "$FLOW" type-at-pos opaque.js 21 19 --strip-root --pretty
printf "opaque.js:21:28 = "
assert_ok "$FLOW" type-at-pos opaque.js 21 28 --strip-root --pretty
printf "opaque.js:24:7 = "
assert_ok "$FLOW" type-at-pos opaque.js 24 7 --strip-root --pretty

# optional.js
printf "optional.js:4:10 = "
assert_ok "$FLOW" type-at-pos optional.js 4 10 --strip-root --pretty
printf "optional.js:7:2 = "
assert_ok "$FLOW" type-at-pos optional.js 7 2 --strip-root --pretty
printf "optional.js:10:11 = "
assert_ok "$FLOW" type-at-pos optional.js 10 11 --strip-root --pretty
printf "optional.js:10:14 = "
assert_ok "$FLOW" type-at-pos optional.js 10 14 --strip-root --pretty
printf "optional.js:14:10 = "
assert_ok "$FLOW" type-at-pos optional.js 14 10 --strip-root --pretty

# stack-overflow-bugfix.js
# This used to cause Stack overflow due to a normalizer bug in Substitution
# with a mapping of the form: A -> Bound(A)
printf "stack-overflow-bugfix.js:14:10 = "
assert_ok "$FLOW" type-at-pos stack-overflow-bugfix.js 5 6 --strip-root --pretty --expand-type-aliases

# recursive.js
printf "recursive.js:3:25 = "
assert_ok "$FLOW" type-at-pos recursive.js 3 25 --strip-root --pretty
printf "recursive.js:6:11 = "
assert_ok "$FLOW" type-at-pos recursive.js 6 11 --strip-root --pretty
printf "recursive.js:13:12 = "
assert_ok "$FLOW" type-at-pos recursive.js 13 12 --strip-root --pretty
printf "recursive.js:23:12 = "
assert_ok "$FLOW" type-at-pos recursive.js 23 12 --strip-root --pretty
printf "recursive.js:38:2 = "
assert_ok "$FLOW" type-at-pos recursive.js 38 2 --strip-root --pretty
printf "recursive.js:41:17 = "
assert_ok "$FLOW" type-at-pos recursive.js 41 17 --strip-root --pretty
printf "recursive.js:58:1 = "
assert_ok "$FLOW" type-at-pos recursive.js 58 1 --strip-root --pretty
printf "recursive.js:60:6 = "
assert_ok "$FLOW" type-at-pos recursive.js 60 6 --strip-root --pretty
printf "recursive.js:60:31 = "
assert_ok "$FLOW" type-at-pos recursive.js 60 31 --strip-root --pretty

# spread.js
printf "spread.js:12:6 = "
assert_ok "$FLOW" type-at-pos spread.js 12 6 --strip-root --pretty
printf "spread.js:13:6 = "
assert_ok "$FLOW" type-at-pos spread.js 13 6 --strip-root --pretty
printf "spread.js:14:6 = "
assert_ok "$FLOW" type-at-pos spread.js 14 6 --strip-root --pretty
printf "spread.js:15:6 = "
assert_ok "$FLOW" type-at-pos spread.js 15 6 --strip-root --pretty
printf "spread.js:16:6 = "
assert_ok "$FLOW" type-at-pos spread.js 16 6 --strip-root --pretty
printf "spread.js:17:6 = "
assert_ok "$FLOW" type-at-pos spread.js 17 6 --strip-root --pretty
printf "spread.js:19:6 = "
assert_ok "$FLOW" type-at-pos spread.js 19 6 --strip-root --pretty
printf "spread.js:22:6 = "
assert_ok "$FLOW" type-at-pos spread.js 22 6 --strip-root --pretty
printf "spread.js:26:6 = "
assert_ok "$FLOW" type-at-pos spread.js 26 6 --strip-root --pretty
printf "spread.js:27:6 = "
assert_ok "$FLOW" type-at-pos spread.js 27 6 --strip-root --pretty
printf "spread.js:28:6 = "
assert_ok "$FLOW" type-at-pos spread.js 28 6 --strip-root --pretty
printf "spread.js:29:6 = "
assert_ok "$FLOW" type-at-pos spread.js 29 6 --strip-root --pretty
printf "spread.js:30:6 = "
assert_ok "$FLOW" type-at-pos spread.js 30 6 --strip-root --pretty
printf "spread.js:31:6 = "
assert_ok "$FLOW" type-at-pos spread.js 31 6 --strip-root --pretty
printf "spread.js:32:6 = "
assert_ok "$FLOW" type-at-pos spread.js 32 6 --strip-root --pretty
printf "spread.js:33:6 = "
assert_ok "$FLOW" type-at-pos spread.js 33 6 --strip-root --pretty
printf "spread.js:34:6 = "
assert_ok "$FLOW" type-at-pos spread.js 34 6 --strip-root --pretty
printf "spread.js:35:6 = "
assert_ok "$FLOW" type-at-pos spread.js 35 6 --strip-root --pretty
printf "spread.js:36:6 = "
assert_ok "$FLOW" type-at-pos spread.js 36 6 --strip-root --pretty --expand-type-aliases
printf "spread.js:37:6 = "
assert_ok "$FLOW" type-at-pos spread.js 37 6 --strip-root --pretty
printf "spread.js:38:6 = "
assert_ok "$FLOW" type-at-pos spread.js 38 6 --strip-root --pretty
printf "spread.js:39:6 = "
assert_ok "$FLOW" type-at-pos spread.js 39 6 --strip-root --pretty
printf "spread.js:40:6 = "
assert_ok "$FLOW" type-at-pos spread.js 40 6 --strip-root --pretty
printf "spread.js:41:6 = "
assert_ok "$FLOW" type-at-pos spread.js 41 6 --strip-root --pretty
printf "spread.js:42:6 = "
assert_ok "$FLOW" type-at-pos spread.js 42 6 --strip-root --pretty
printf "spread.js:43:6 = "
assert_ok "$FLOW" type-at-pos spread.js 43 6 --strip-root --pretty
printf "spread.js:44:6 = "
assert_ok "$FLOW" type-at-pos spread.js 44 6 --strip-root --pretty
printf "spread.js:45:6 = "
assert_ok "$FLOW" type-at-pos spread.js 45 6 --strip-root --pretty
printf "spread.js:46:6 = "
assert_ok "$FLOW" type-at-pos spread.js 46 6 --strip-root --pretty

# subst.js
printf "subst.js:13:7 = "
assert_ok "$FLOW" type-at-pos subst.js 13 7 --strip-root --pretty
printf "subst.js:14:7 = "
assert_ok "$FLOW" type-at-pos subst.js 14 7 --strip-root --pretty
printf "subst.js:17:7 = "
assert_ok "$FLOW" type-at-pos subst.js 17 7 --strip-root --pretty
printf "subst.js:18:7 = "
assert_ok "$FLOW" type-at-pos subst.js 18 7 --strip-root --pretty
printf "subst.js:21:7 = "
assert_ok "$FLOW" type-at-pos subst.js 21 7 --strip-root --pretty
printf "subst.js:22:7 = "
assert_ok "$FLOW" type-at-pos subst.js 22 7 --strip-root --pretty


# type-alias.js
printf "type-alias.js:3:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 3 6 --strip-root --pretty
printf "type-alias.js:4:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 4 6 --strip-root --pretty
printf "type-alias.js:5:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 5 6 --strip-root --pretty
printf "type-alias.js:6:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 6 6 --strip-root --pretty
printf "type-alias.js:7:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 7 6 --strip-root --pretty
printf "type-alias.js:7:6 (--expand-type-aliases) = "
assert_ok "$FLOW" type-at-pos type-alias.js 7 6 --strip-root --pretty --expand-type-aliases
printf "type-alias.js:8:6 = "
assert_ok "$FLOW" type-at-pos type-alias.js 8 6 --strip-root --pretty
printf "type-alias.js:12:12 "
assert_ok "$FLOW" type-at-pos type-alias.js 12 12 --strip-root --pretty
printf "type-alias.js:12:29 "
assert_ok "$FLOW" type-at-pos type-alias.js 12 29 --strip-root --pretty

# Test interaction with RPolyTest
printf "type-alias.js:15:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 15 8 --strip-root --pretty
printf "type-alias.js:16:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 16 8 --strip-root --pretty
printf "type-alias.js:17:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 17 8 --strip-root --pretty
printf "type-alias.js:18:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 18 8 --strip-root --pretty
printf "type-alias.js:19:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 19 8 --strip-root --pretty
printf "type-alias.js:20:8 "
assert_ok "$FLOW" type-at-pos type-alias.js 20 8 --strip-root --pretty

printf "type-alias.js:24:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 24 6 --strip-root --pretty --expand-type-aliases
printf "type-alias.js:25:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 25 6 --strip-root --pretty --expand-type-aliases
printf "type-alias.js:27:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 27 6 --strip-root --pretty --expand-type-aliases
printf "type-alias.js:29:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 29 6 --strip-root --pretty --expand-type-aliases
printf "type-alias.js:31:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 31 6 --strip-root --pretty --expand-type-aliases

printf "type-alias.js:34:6 "
assert_ok "$FLOW" type-at-pos type-alias.js 34 6 --strip-root --pretty --expand-json-output

# type-destructor.js
printf "type-destructor.js:3:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 3 6 --strip-root --pretty
printf "type-destructor.js:4:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 4 6 --strip-root --pretty
printf "type-destructor.js:5:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 5 6 --strip-root --pretty
printf "type-destructor.js:8:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 8 6 --strip-root --pretty
printf "type-destructor.js:10:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 10 6 --strip-root --pretty
printf "type-destructor.js:12:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 12 6 --strip-root --pretty
printf "type-destructor.js:13:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 15 6 --strip-root --pretty
printf "type-destructor.js:16:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 16 6 --strip-root --pretty
printf "type-destructor.js:17:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 17 6 --strip-root --pretty
printf "type-destructor.js:19:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 19 6 --strip-root --pretty
printf "type-destructor.js:20:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 20 6 --strip-root --pretty
printf "type-destructor.js:21:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 21 6 --strip-root --pretty
printf "type-destructor.js:23:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 23 6 --strip-root --pretty
printf "type-destructor.js:27:5 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 27 5 --strip-root --pretty
printf "type-destructor.js:28:5 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 28 5 --strip-root --pretty
printf "type-destructor.js:29:5 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 29 5 --strip-root --pretty
printf "type-destructor.js:33:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 33 6 --strip-root --pretty
printf "type-destructor.js:34:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 34 6 --strip-root --pretty
printf "type-destructor.js:36:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 36 6 --strip-root --pretty
printf "type-destructor.js:37:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 37 6 --strip-root --pretty
printf "type-destructor.js:41:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 41 6 --strip-root --pretty
printf "type-destructor.js:42:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 42 6 --strip-root --pretty
printf "type-destructor.js:44:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 44 6 --strip-root --pretty
printf "type-destructor.js:45:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 45 6 --strip-root --pretty
printf "type-destructor.js:47:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 47 6 --strip-root --pretty
printf "type-destructor.js:48:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 48 6 --strip-root --pretty
printf "type-destructor.js:62:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 62 6 --strip-root --pretty
printf "type-destructor.js:63:6 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 63 6 --strip-root --pretty
printf "type-destructor.js:68:13 = "
assert_ok "$FLOW" type-at-pos type-destructor.js 68 13 --strip-root --pretty

# type-destructor-trigger.js
printf "type-destructor-trigger.js:11:7 = "
assert_ok "$FLOW" type-at-pos type-destructor-trigger.js 11 7 --strip-root --pretty

# unions.js
printf "unions.js:9:3 = "
assert_ok "$FLOW" type-at-pos unions.js 9 3 --strip-root --pretty
printf "unions.js:15:2 = "
assert_ok "$FLOW" type-at-pos unions.js 15 2 --strip-root --pretty
printf "unions.js:24:3 = "
assert_ok "$FLOW" type-at-pos unions.js 24 3 --strip-root --pretty
printf "unions.js:43:3 = "
assert_ok "$FLOW" type-at-pos unions.js 43 3 --strip-root --pretty
printf "unions.js:44:3 = "
assert_ok "$FLOW" type-at-pos unions.js 44 3 --strip-root --pretty
printf "unions.js:49:1 = "
assert_ok "$FLOW" type-at-pos unions.js 49 1 --strip-root --pretty
printf "unions.js:52:1 = "
assert_ok "$FLOW" type-at-pos unions.js 52 1 --strip-root --pretty
printf "unions.js:57:5 = "
assert_ok "$FLOW" type-at-pos unions.js 57 5 --strip-root --pretty
printf "unions.js:59:18 = "
assert_ok "$FLOW" type-at-pos unions.js 59 18 --strip-root --pretty

# tparam_defaults.js
printf "tparam_defaults.js:11:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 11 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:12:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 12 8 --strip-root --omit-typearg-defaults

printf "tparam_defaults.js:14:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 14 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:15:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 15 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:16:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 16 8 --strip-root --omit-typearg-defaults

printf "tparam_defaults.js:18:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 18 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:19:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 19 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:20:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 20 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:21:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 21 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:22:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 22 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:24:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 24 8 --strip-root --omit-typearg-defaults
printf "tparam_defaults.js:25:8:\n"
assert_ok "$FLOW" type-at-pos tparam_defaults.js 25 8 --strip-root --omit-typearg-defaults

# utility.js
printf "utility.js:3:6:\n"
assert_ok "$FLOW" type-at-pos utility.js 3 6 --strip-root
