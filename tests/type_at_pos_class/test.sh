#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# annot_t.js
printf "annot_t.js:9:21 = "
assert_ok "$FLOW" type-at-pos annot_t.js 9 21 --strip-root --pretty

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

# class-1.js
# TODO this is not the ideal type
printf "class-1.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-1.js 4 3 --strip-root --pretty
printf "class-1.js:8:3 = "
assert_ok "$FLOW" type-at-pos class-1.js 8 3 --strip-root --pretty

# class-2.js
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

# class-3.js
printf "class-3.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-3.js 4 3 --strip-root --pretty
printf "class-3.js:9:9 = "
assert_ok "$FLOW" type-at-pos class-3.js 9 9 --strip-root --pretty
printf "class-3.js:10:9 = "
assert_ok "$FLOW" type-at-pos class-3.js 10 9 --strip-root --pretty

# class-4.js
printf "class-4.js:9:6 = "
assert_ok "$FLOW" type-at-pos class-4.js 9 6 --strip-root --pretty

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

# declare_class.js
printf "declare_class.js:3:15 = "
assert_ok "$FLOW" type-at-pos declare_class.js 3 15 --strip-root
printf "declare_class.js:4:5 = "
assert_ok "$FLOW" type-at-pos declare_class.js 4 5 --strip-root
printf "declare_class.js:6:15 = "
assert_ok "$FLOW" type-at-pos declare_class.js 6 15 --strip-root
printf "declare_class.js:7:5 = "
assert_ok "$FLOW" type-at-pos declare_class.js 7 5 --strip-root

# FluxStore.js
printf "FluxStore.js:14:5 = "
assert_ok "$FLOW" type-at-pos FluxStore.js 14 5 --strip-root --pretty

# implements.js
printf "implements.js:7:8 = "
assert_ok "$FLOW" type-at-pos implements.js 4 23 --strip-root --pretty

# multi-inheritance.js
printf "multi-inheritance.js:6:7 = "
assert_ok "$FLOW" type-at-pos multi-inheritance.js 6 7 --strip-root --pretty

# type-utils.js
printf "type-utils.js:5:13 "
assert_ok "$FLOW" type-at-pos type-utils.js 5 13 --strip-root --pretty --expand-json-output
printf "type-utils.js:6:13 "
assert_ok "$FLOW" type-at-pos type-utils.js 6 13 --strip-root --pretty --expand-json-output
printf "type-utils.js:10:13 "
assert_ok "$FLOW" type-at-pos type-utils.js 10 13 --strip-root --pretty
printf "type-utils.js:11:13 "
assert_ok "$FLOW" type-at-pos type-utils.js 11 13 --strip-root --pretty
printf "type-utils.js:12:13 "
assert_ok "$FLOW" type-at-pos type-utils.js 12 13 --strip-root --pretty

# default.js
printf "default.js:4:17 "
assert_ok "$FLOW" type-at-pos default.js 4 17 --strip-root --pretty
printf "default.js:5:18 "
assert_ok "$FLOW" type-at-pos default.js 5 18 --strip-root --pretty
