#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nRename of an object property:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 3 21 --rename newName

printf "\nRename of a local variable used in object shorthand:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 6 9 --rename newName

printf "\nRename of a local variable bound by destructuring shorthand:\n"
assert_ok "$FLOW" refactor --pretty --strip-root objects.js 12 10 --rename newName
