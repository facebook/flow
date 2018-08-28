#!/bin/bash

## To avoid regression ensure that all of these do not return (unknown)

# annot_t.js
printf "annot_t.js:9:21 = "
assert_ok "$FLOW" type-at-pos annot_t.js 9 21 --strip-root --pretty

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

# new-array.js
printf "new-array.js:3:15 = "
assert_ok "$FLOW" type-at-pos new-array.js 3 15 --strip-root --pretty

# class-0.js
printf "class-0.js:3:7 = "
assert_ok "$FLOW" type-at-pos class-0.js 3 7 --strip-root --pretty
printf "class-0.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-0.js 4 3 --strip-root --pretty
printf "class-0.js:4:10 = "
assert_ok "$FLOW" type-at-pos class-0.js 4 10 --strip-root --pretty
printf "class-0.js:9:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 9 5 --strip-root --pretty
printf "class-0.js:12:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 12 5 --strip-root --pretty
printf "class-0.js:15:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 15 5 --strip-root --pretty
printf "class-0.js:21:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 21 5 --strip-root --pretty
printf "class-0.js:24:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 24 5 --strip-root --pretty

#class-1.js
# TODO this is not the ideal type
printf "class-1.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-1.js 4 3 --strip-root --pretty
printf "class-1.js:8:3 = "
assert_ok "$FLOW" type-at-pos class-1.js 8 3 --strip-root --pretty

#class-2.js
printf "class-2.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-2.js 4 3 --strip-root --pretty
printf "class-2.js:9:9 = "
assert_ok "$FLOW" type-at-pos class-2.js 9 9 --strip-root --pretty
printf "class-2.js:10:9 = "
assert_ok "$FLOW" type-at-pos class-2.js 10 9 --strip-root --pretty
printf "class-2.js:12:7 = "
assert_ok "$FLOW" type-at-pos class-2.js 12 7 --strip-root --pretty
printf "class-2.js:13:7 = "
assert_ok "$FLOW" type-at-pos class-2.js 13 7 --strip-root --pretty

#class-3.js
printf "class-3.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-3.js 4 3 --strip-root --pretty
printf "class-3.js:9:9 = "
assert_ok "$FLOW" type-at-pos class-3.js 9 9 --strip-root --pretty
printf "class-3.js:10:9 = "
assert_ok "$FLOW" type-at-pos class-3.js 10 9 --strip-root --pretty

# class-bound.js
printf "class-bound.js:4:6 = "
assert_ok "$FLOW" type-at-pos class-bound.js 4 6 --strip-root --pretty

# class-getters-setters.js
printf "class-getters-setters.js:6:7 = "
assert_ok "$FLOW" type-at-pos class-getters-setters.js 6 7 --strip-root --pretty
printf "class-getters-setters.js:9:7 = "
assert_ok "$FLOW" type-at-pos class-getters-setters.js 9 7 --strip-root --pretty

# class-poly-0.js
printf "class-poly-0.js:5:7 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 5 7 --strip-root --pretty
printf "class-poly-0.js:5:9 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 5 9 --strip-root --pretty
printf "class-poly-0.js:10:26 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 10 26 --strip-root --pretty
# TODO constructor
# printf "class-poly-0.js:11:10 = "
# assert_ok "$FLOW" type-at-pos class-poly-0.js 11 10 --strip-root --pretty
printf "class-poly-0.js:12:7 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 12 7 --strip-root --pretty
printf "class-poly-0.js:16:7 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 16 7 --strip-root --pretty
printf "class-poly-0.js:16:10 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 16 10 --strip-root --pretty
printf "class-poly-0.js:17:7 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 17 7 --strip-root --pretty

#class-poly-1.js
printf "class-poly-1.js:9:5 = "
assert_ok "$FLOW" type-at-pos class-poly-1.js 9 5 --strip-root --pretty
printf "class-poly-1.js:9:11 = "
assert_ok "$FLOW" type-at-pos class-poly-1.js 9 11 --strip-root --pretty

# class-statics.js
printf "class-statics.js:4:10 = "
assert_ok "$FLOW" type-at-pos class-statics.js 4 10 --strip-root --pretty
printf "class-statics.js:8:10 = "
assert_ok "$FLOW" type-at-pos class-statics.js 8 10 --strip-root --pretty
printf "class-statics.js:9:7 = "
assert_ok "$FLOW" type-at-pos class-statics.js 9 7 --strip-root --pretty
printf "class-statics.js:11:8 = "
assert_ok "$FLOW" type-at-pos class-statics.js 11 8 --strip-root --pretty
printf "class-statics.js:16:5 = "
assert_ok "$FLOW" type-at-pos class-statics.js 16 5 --strip-root --pretty
printf "class-statics.js:17:5 = "
assert_ok "$FLOW" type-at-pos class-statics.js 17 5 --strip-root --pretty
# NOTE here Flow infers 'this', even though this is a static member
printf "class-statics.js:20:11 = "
assert_ok "$FLOW" type-at-pos class-statics.js 20 11 --strip-root --pretty

# class-statics-poly.js
printf "class-statics-poly.js:4:10 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 4 10 --strip-root --pretty
printf "class-statics-poly.js:8:10 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 8 10 --strip-root --pretty
# TODO the type 'Class<A>' is not parseable when 'A' is polymorphic
printf "class-statics-poly.js:9:7 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 9 7 --strip-root --pretty
printf "class-statics-poly.js:11:8 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 11 8 --strip-root --pretty
printf "class-statics-poly.js:16:5 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 16 5 --strip-root --pretty
printf "class-statics-poly.js:17:5 = "
assert_ok "$FLOW" type-at-pos class-statics-poly.js 17 5 --strip-root --pretty

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

# facebookism.js
printf "facebookism.js:3:8 = "
assert_ok "$FLOW" type-at-pos facebookism.js 3 8 --strip-root --pretty
# TODO `require`
# printf "facebookism.js:3:14 = "
# assert_ok "$FLOW" type-at-pos facebookism.js 3 14 --strip-root --pretty

# function.js
printf "function.js:4:3 = "
assert_ok "$FLOW" type-at-pos function.js 4 3 --strip-root --pretty
printf "function.js:8:3 = "
assert_ok "$FLOW" type-at-pos function.js 8 3 --strip-root --pretty
printf "function.js:12:3 = "
assert_ok "$FLOW" type-at-pos function.js 12 3 --strip-root --pretty
printf "function.js:16:3 = "
assert_ok "$FLOW" type-at-pos function.js 16 3 --strip-root --pretty

# function-poly-0.js
printf "function-poly-0.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 3 10 --strip-root --pretty
printf "function-poly-0.js:3:30 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 3 30 --strip-root --pretty
printf "function-poly-0.js:4:7 = "
assert_ok "$FLOW" type-at-pos function-poly-0.js 4 7 --strip-root --pretty

# function-poly-1.js
printf "function-poly-1.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 3 10 --strip-root --pretty
printf "function-poly-1.js:3:3 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 3 33 --strip-root --pretty
printf "function-poly-1.js:4:7 = "
assert_ok "$FLOW" type-at-pos function-poly-1.js 4 7 --strip-root --pretty

# function-poly-2.js
printf "function-poly-2.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 3 10 --strip-root --pretty
printf "function-poly-2.js:4:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 4 12 --strip-root --pretty
printf "function-poly-2.js:5:5 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 5 5 --strip-root --pretty
printf "function-poly-2.js:6:5 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 6 5 --strip-root --pretty
printf "function-poly-2.js:7:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 7 12 --strip-root --pretty
printf "function-poly-2.js:9:13 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 9 13 --strip-root --pretty
printf "function-poly-2.js:11:12 = "
assert_ok "$FLOW" type-at-pos function-poly-2.js 11 12 --strip-root --pretty

# function-poly-3.js
printf "function-poly-3.js:8:1 = "
assert_ok "$FLOW" type-at-pos function-poly-3.js 8 1 --strip-root --pretty

# function-poly-4.js
printf "function-poly-4.js:3:11 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 3 11 --strip-root --pretty
printf "function-poly-4.js:7:7 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 7 7 --strip-root --pretty
printf "function-poly-4.js:9:7 = "
assert_ok "$FLOW" type-at-pos function-poly-4.js 9 7 --strip-root --pretty

# function-poly-5.js
printf "function-poly-5.js:3:10 = "
assert_ok "$FLOW" type-at-pos function-poly-5.js 3 10 --strip-root --pretty

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
printf "generics.js:26:1 = "
assert_ok "$FLOW" type-at-pos generics.js 26 1 --strip-root --pretty
printf "generics.js:30:13 = "
assert_ok "$FLOW" type-at-pos generics.js 30 13 --strip-root --pretty

# implements.js
printf "implements.js:7:8 = "
assert_ok "$FLOW" type-at-pos implements.js 4 23 --strip-root --pretty

# import_lib.js
printf "import_lib.js:7:8 = "
assert_ok "$FLOW" type-at-pos import_lib.js 7 8 --strip-root --pretty
printf "import_lib.js:7:25 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import_lib.js 7 25 --strip-root --pretty --expand-json-output

# import_lib_named.js
printf "import_lib_named.js:3:15 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import_lib_named.js 3 15 --strip-root --pretty --expand-json-output
printf "import_lib_named.js:3:27 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import_lib_named.js 3 27 --strip-root --pretty --expand-json-output

# interface.js
printf "interface.js:3:12 = "
assert_ok "$FLOW" type-at-pos interface.js 3 12 --strip-root --pretty
printf "interface.js:9:15 = "
assert_ok "$FLOW" type-at-pos interface.js 9 15 --strip-root --pretty
printf "interface.js:9:19 = "
assert_ok "$FLOW" type-at-pos interface.js 9 19 --strip-root --pretty
# # TODO: report specialized type
# printf "interface.js:10:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 10 6 --strip-root --pretty
# printf "interface.js:11:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 11 6 --strip-root --pretty
# printf "interface.js:13:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 13 6 --strip-root --pretty
printf "interface.js:17:7 = "
assert_ok "$FLOW" type-at-pos interface.js 17 7 --strip-root --pretty
printf "interface.js:18:7 = "
assert_ok "$FLOW" type-at-pos interface.js 18 7 --strip-root --pretty

# declare_class.js
printf "declare_class.js:3:15 = "
assert_ok "$FLOW" type-at-pos declare_class.js 3 15 --strip-root --pretty

# mixed.js
printf "mixed.js:18:17 = "
assert_ok "$FLOW" type-at-pos mixed.js 18 17 --strip-root --pretty

# exports.js
printf "exports.js:3:24 = "
assert_ok "$FLOW" type-at-pos exports.js 3 24 --strip-root --pretty
printf "exports.js:5:25 = "
assert_ok "$FLOW" type-at-pos exports.js 5 25 --strip-root --pretty

# module-export.js
printf "module-export.js:7:13 = "
assert_ok "$FLOW" type-at-pos module-export.js 7 13 --strip-root --pretty

# module-import.js
printf "module-import.js:3:7 = "
assert_ok "$FLOW" type-at-pos module-import.js 3 7 --strip-root --pretty

# import-default.js
printf "import-default.js:3:16 = "
assert_ok "$FLOW" type-at-pos import-default.js 3 16 --strip-root --pretty

# import-typeof-class.js
printf "import-typeof-class.js:6:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 6 16 --strip-root --pretty --expand-json-output
printf "import-typeof-class.js:7:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 7 16 --strip-root --pretty --expand-json-output

# object.js
printf "object.js:3:15 = "
assert_ok "$FLOW" type-at-pos object.js 3 15 --strip-root --pretty
printf "object.js:3:19 = "
assert_ok "$FLOW" type-at-pos object.js 3 19 --strip-root --pretty
printf "object.js:3:24 = "
assert_ok "$FLOW" type-at-pos object.js 3 24 --strip-root --pretty
printf "object.js:3:29 = "
assert_ok "$FLOW" type-at-pos object.js 3 29 --strip-root --pretty
printf "object.js:3:40 = "
assert_ok "$FLOW" type-at-pos object.js 3 40 --strip-root --pretty
printf "object.js:6:5 = "
assert_ok "$FLOW" type-at-pos object.js 6 5 --strip-root --pretty
printf "object.js:6:7 = " # TODO can we do better with duplication?
assert_ok "$FLOW" type-at-pos object.js 6 7 --strip-root --pretty
printf "object.js:7:10 = "
assert_ok "$FLOW" type-at-pos object.js 7 10 --strip-root --pretty
printf "object.js:7:12 = "
assert_ok "$FLOW" type-at-pos object.js 7 12 --strip-root --pretty
printf "object.js:8:14 = "
assert_ok "$FLOW" type-at-pos object.js 8 14 --strip-root --pretty
printf "object.js:8:16 = "
assert_ok "$FLOW" type-at-pos object.js 8 16 --strip-root --pretty
printf "object.js:9:18 = "
assert_ok "$FLOW" type-at-pos object.js 9 18 --strip-root --pretty
printf "object.js:9:34 = "
assert_ok "$FLOW" type-at-pos object.js 9 34 --strip-root --pretty
printf "object.js:15:3 = "
assert_ok "$FLOW" type-at-pos object.js 15 3 --strip-root --pretty
printf "object.js:16:3 = "
assert_ok "$FLOW" type-at-pos object.js 16 3 --strip-root --pretty
printf "object.js:19:3 = "
assert_ok "$FLOW" type-at-pos object.js 19 3 --strip-root --pretty
printf "object.js:19:7 = "
assert_ok "$FLOW" type-at-pos object.js 19 7 --strip-root --pretty
printf "object.js:20:7 = "
assert_ok "$FLOW" type-at-pos object.js 20 7 --strip-root --pretty
printf "object.js:21:7 = "
assert_ok "$FLOW" type-at-pos object.js 21 7 --strip-root --pretty
printf "object.js:22:7 = "
assert_ok "$FLOW" type-at-pos object.js 22 7 --strip-root --pretty
printf "object.js:35:1 = "
assert_ok "$FLOW" type-at-pos object.js 35 1 --strip-root --pretty

# object-resolution.js
printf "object-resolution.js:5:2 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 5 2 --strip-root --pretty
printf "object-resolution.js:10:2 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 10 2 --strip-root --pretty
printf "object-resolution.js:13:5 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 13 5 --strip-root --pretty

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

# predicates.js
# printf "predicates.js:4:12 (null) = "
# assert_ok "$FLOW" type-at-pos predicates.js 4 12 --strip-root --pretty
printf "predicates.js - undefined: "
assert_ok "$FLOW" type-at-pos predicates.js 5 12 --strip-root --pretty
printf "predicates.js - Array: "
assert_ok "$FLOW" type-at-pos predicates.js 6 6 --strip-root --pretty
printf "predicates.js - isArray: "
assert_ok "$FLOW" type-at-pos predicates.js 6 15 --strip-root --pretty
printf "predicates.js - y (refined obj): "
assert_ok "$FLOW" type-at-pos predicates.js 8 5 --strip-root --pretty
printf "predicates.js - if (y.FOO) obj: "
assert_ok "$FLOW" type-at-pos predicates.js 9 5 --strip-root --pretty
printf "predicates.js - if (y.FOO) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 9 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == '') obj: "
assert_ok "$FLOW" type-at-pos predicates.js 10 5 --strip-root --pretty
printf "predicates.js - if (y.FOO == '') prop: "
assert_ok "$FLOW" type-at-pos predicates.js 10 8 --strip-root --pretty
printf "predicates.js - if (y.FOO === '') obj: "
assert_ok "$FLOW" type-at-pos predicates.js 11 5 --strip-root --pretty
printf "predicates.js - if (y.FOO === '') prop: "
assert_ok "$FLOW" type-at-pos predicates.js 11 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == null) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 12 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == undefined) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 13 8 --strip-root --pretty
printf "predicates.js - if (Array.isArray(y.FOO)): "
assert_ok "$FLOW" type-at-pos predicates.js 14 22 --strip-root --pretty

# react_component.js
printf "react_component.js:3:9 = "
assert_ok "$FLOW" type-at-pos react_component.js 3 9 --strip-root --pretty
printf "react_component.js:13:33 = "
assert_ok "$FLOW" type-at-pos react_component.js 13 33 --strip-root --pretty
printf "react_component.js:18:17 = "
assert_ok "$FLOW" type-at-pos react_component.js 18 17 --strip-root --pretty
printf "react_component.js:31:7 = "
assert_ok "$FLOW" type-at-pos react_component.js 31 7 --strip-root --pretty --expand-json-output
printf "react_component.js:32:13 = "
assert_ok "$FLOW" type-at-pos react_component.js 32 13 --strip-root --pretty --expand-json-output
printf "react_component.js:32:29 = "
assert_ok "$FLOW" type-at-pos react_component.js 32 29 --strip-root --pretty --expand-json-output

# react.js
printf "react.js:2:7 = "
assert_ok "$FLOW" type-at-pos react.js 2 7 --strip-root --pretty

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

# refinement.js
printf "refinement.js:7:25 = "
assert_ok "$FLOW" type-at-pos refinement.js 7 25 --strip-root --pretty
printf "refinement.js:8:25 = "
assert_ok "$FLOW" type-at-pos refinement.js 8 25 --strip-root --pretty

# require-class.js
printf "require-class.js:5:16 = "
assert_ok "$FLOW" type-at-pos require-class.js 5 16 --strip-root --expand-json-output --pretty
printf "require-class.js:6:16 = "
assert_ok "$FLOW" type-at-pos require-class.js 6 16 --strip-root --expand-json-output --pretty

# test.js
printf "test.js:5:1 = "
assert_ok "$FLOW" type-at-pos test.js 5 1 --strip-root --pretty
printf "test.js:8:7 = "
assert_ok "$FLOW" type-at-pos test.js 8 7 --strip-root --pretty
printf "test.js:10:7 = "
assert_ok "$FLOW" type-at-pos test.js 10 7 --strip-root --pretty
printf "test.js:12:7 = "
assert_ok "$FLOW" type-at-pos test.js 12 7 --strip-root --pretty
printf "test.js:14:7 = "
assert_ok "$FLOW" type-at-pos test.js 14 7 --strip-root --pretty

# templates.js
# NOTE: not supported any more
# printf "templates.js:2:7 = "
# assert_ok "$FLOW" type-at-pos templates.js 2 7 --strip-root --pretty

# trycatch.js
# TODO track type reaching catch variable
# printf "trycatch.js:5:10 = "
# assert_ok "$FLOW" type-at-pos trycatch.js 5 10 --strip-root --pretty

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
assert_ok "$FLOW" type-at-pos type-destructor.js 13 6 --strip-root --pretty
printf "type-destructor.js:15:6 = "
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
assert_ok "$FLOW" type-at-pos type-destructor.js 28 5 --strip-root --pretty
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

# optional_chaining.js
printf "optional_chaining.js:16:7 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 7 --strip-root --pretty
printf "optional_chaining.js:16:11 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 11 --strip-root --pretty
printf "optional_chaining.js:16:16 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 16 --strip-root --pretty
printf "optional_chaining.js:16:20 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 20 --strip-root --pretty
printf "optional_chaining.js:16:24 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 24 --strip-root --pretty

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
