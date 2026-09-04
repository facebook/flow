#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# annot_t.js
printf "annot_t.js:9:23 = "
assert_ok "$FLOW" type-at-pos annot_t.js 9 23 --strip-root --pretty

# class-0.js
printf "class-0.js:3:7 = "
assert_ok "$FLOW" type-at-pos class-0.js 3 7 --strip-root --pretty
printf "class-0.js:4:3 = "
assert_ok "$FLOW" type-at-pos class-0.js 4 3 --strip-root --pretty
printf "class-0.js:4:10 = "
assert_ok "$FLOW" type-at-pos class-0.js 4 10 --strip-root --pretty
printf "class-0.js:12:5 = "
assert_ok "$FLOW" type-at-pos class-0.js 12 5 --strip-root --pretty
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
printf "class-poly-0.js:10:33 = "
assert_ok "$FLOW" type-at-pos class-poly-0.js 10 33 --strip-root --pretty
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
printf "class-statics.js:20:19 = "
assert_ok "$FLOW" type-at-pos class-statics.js 20 19 --strip-root --pretty

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
printf "type-utils.js:5:15 "
assert_ok "$FLOW" type-at-pos type-utils.js 5 15 --strip-root --pretty --expand-json-output
printf "type-utils.js:6:15 "
assert_ok "$FLOW" type-at-pos type-utils.js 6 15 --strip-root --pretty --expand-json-output
printf "type-utils.js:10:15 "
assert_ok "$FLOW" type-at-pos type-utils.js 10 15 --strip-root --pretty
printf "type-utils.js:11:15 "
assert_ok "$FLOW" type-at-pos type-utils.js 11 15 --strip-root --pretty
printf "type-utils.js:12:15 "
assert_ok "$FLOW" type-at-pos type-utils.js 12 15 --strip-root --pretty

# default.js
printf "default.js:4:17 "
assert_ok "$FLOW" type-at-pos default.js 4 17 --strip-root --pretty
printf "default.js:5:18 "
assert_ok "$FLOW" type-at-pos default.js 5 18 --strip-root --pretty

# constructor.js
printf "constructor.js:4:7 "
assert_ok "$FLOW" type-at-pos constructor.js 4 7 --strip-root --pretty

# this.js
printf "this.js:5:13 "
assert_ok "$FLOW" type-at-pos this.js 5 13 --strip-root
# function_this.js
printf "function_this.js:2:1 "
assert_ok "$FLOW" type-at-pos function_this.js 2 1 --strip-root --pretty
printf "function_this.js:5:1 "
assert_ok "$FLOW" type-at-pos function_this.js 5 1 --strip-root --pretty
printf "function_this.js:8:1 "
assert_ok "$FLOW" type-at-pos function_this.js 8 1 --strip-root --pretty

# Declaration framing. The queries above ask for `--pretty`, whose JSON payload
# is a bare type by design, so these repeat a few member positions in friendly
# mode where the declaration head is rendered.
printf "class-0.js:4:3 (framed) = "
assert_ok "$FLOW" type-at-pos class-0.js 4 3 --strip-root
printf "class-statics.js:4:10 (framed) = "
assert_ok "$FLOW" type-at-pos class-statics.js 4 10 --strip-root
printf "class-getters-setters.js:6:7 (framed) = "
assert_ok "$FLOW" type-at-pos class-getters-setters.js 6 7 --strip-root
printf "class-getters-setters.js:9:7 (framed) = "
assert_ok "$FLOW" type-at-pos class-getters-setters.js 9 7 --strip-root
printf "FluxStore.js:5:3 (framed) = "
assert_ok "$FLOW" type-at-pos FluxStore.js 5 3 --strip-root
# A constructor is reported as its class, so it gets no member framing.
printf "constructor.js:4:7 (framed) = "
assert_ok "$FLOW" type-at-pos constructor.js 4 7 --strip-root

# declare_class_members.js
printf "declare_class_members.js:8:3 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 8 3 --strip-root
printf "declare_class_members.js:9:3 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 9 3 --strip-root
printf "declare_class_members.js:10:7 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 10 7 --strip-root
printf "declare_class_members.js:11:7 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 11 7 --strip-root
printf "declare_class_members.js:12:10 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 12 10 --strip-root
printf "declare_class_members.js:13:3 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 13 3 --strip-root
printf "declare_class_members.js:13:12 (framed) = "
assert_ok "$FLOW" type-at-pos declare_class_members.js 13 12 --strip-root

# member_reference.js
# A reference to a member is framed as the declaration it resolves to, which
# takes expanding the receiver's type: the property's own type looks the same
# whether it was declared as a field, a method, or an accessor.
printf "member_reference.js:14:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 14 3 --strip-root
printf "member_reference.js:15:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 15 3 --strip-root
printf "member_reference.js:16:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 16 3 --strip-root
printf "member_reference.js:17:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 17 3 --strip-root
printf "member_reference.js:18:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 18 3 --strip-root
printf "member_reference.js:19:9 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 19 9 --strip-root
printf "member_reference.js:23:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 23 3 --strip-root
printf "member_reference.js:24:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 24 3 --strip-root
printf "member_reference.js:25:3 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference.js 25 3 --strip-root

# member_reference_eval.js
# The receiver of a member access can itself be a type destructor. Expanding its
# members forces evaluation, so the property's kind is still recovered; the
# qualifier comes off the unevaluated receiver.
printf "member_reference_eval.js:18:9 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 18 9 --strip-root
printf "member_reference_eval.js:19:9 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 19 9 --strip-root
printf "member_reference_eval.js:23:7 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 23 7 --strip-root
printf "member_reference_eval.js:24:7 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 24 7 --strip-root
printf "member_reference_eval.js:27:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 27 10 --strip-root
printf "member_reference_eval.js:28:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 28 10 --strip-root
printf "member_reference_eval.js:31:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 31 10 --strip-root
printf "member_reference_eval.js:32:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 32 10 --strip-root
printf "member_reference_eval.js:33:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 33 10 --strip-root
printf "member_reference_eval.js:34:10 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 34 10 --strip-root
printf "member_reference_eval.js:37:8 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 37 8 --strip-root
printf "member_reference_eval.js:38:8 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 38 8 --strip-root
printf "member_reference_eval.js:41:8 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 41 8 --strip-root
printf "member_reference_eval.js:44:13 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 44 13 --strip-root
printf "member_reference_eval.js:45:13 (framed) = "
assert_ok "$FLOW" type-at-pos member_reference_eval.js 45 13 --strip-root

# private_members.js
# A private member is framed like any other, under the name it is written with:
# the `#` is part of what hover reports, though the parser drops it from the
# name it records. A reference resolves through the enclosing class body rather
# than the receiver's type, which does not carry private members at all.
printf "private_members.js:4:3 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 4 3 --strip-root
printf "private_members.js:5:3 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 5 3 --strip-root
printf "private_members.js:6:7 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 6 7 --strip-root
printf "private_members.js:7:10 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 7 10 --strip-root
printf "private_members.js:10:10 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 10 10 --strip-root
printf "private_members.js:11:10 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 11 10 --strip-root
printf "private_members.js:12:10 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 12 10 --strip-root
printf "private_members.js:13:7 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 13 7 --strip-root
# A second class reusing the name resolves to its own declaration.
printf "private_members.js:18:3 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 18 3 --strip-root
printf "private_members.js:20:17 (framed) = "
assert_ok "$FLOW" type-at-pos private_members.js 20 17 --strip-root
