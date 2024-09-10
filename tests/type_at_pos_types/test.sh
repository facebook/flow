#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "any.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root any.js

printf "array.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root array.js

printf "destructuring.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root destructuring.js

printf "generics.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root generics.js

printf "ill-formed.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root ill-formed.js

printf "implicit-instantiation.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root implicit-instantiation.js

printf "interface.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root interface.js

printf "intersections.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root intersections.js

printf "mixed.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root mixed.js

printf "callable-object.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root callable-object.js

printf "opaque.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root opaque.js

printf "optional.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root optional.js

printf "recursive.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root recursive.js

printf "subst.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root subst.js

printf "type-alias.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root type-alias.js

printf "type-destructor-trigger.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root type-destructor-trigger.js

printf "tuple.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root tuple.js

printf "unions.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root unions.js

printf "tparam_defaults.js\n"
"$FLOW" inlay-hint_unstable_exposed_for_testing --strip-root tparam_defaults.js --omit-typearg-defaults
