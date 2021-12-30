#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "\nTrace \`ParentFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 8 1
assert_ok "$FLOW" get-def --strip-root main.js 7 3
assert_ok "$FLOW" get-def --strip-root Parent.js 4 19

printf "\nTrace \`Parent2\` back to its def (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 13 1
assert_ok "$FLOW" get-def --strip-root main.js 12 1
assert_ok "$FLOW" get-def --strip-root main.js 12 14

printf "\nTrace \`Parent3\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 17 1

printf "\nTrace \`Parent\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 5
assert_ok "$FLOW" get-def --strip-root main.js 3 14

printf "\nTrace \`NonDestructuredFoo\` back to its def\n"
assert_ok "$FLOW" get-def --strip-root main.js 21 2
assert_ok "$FLOW" get-def --strip-root main.js 20 5
assert_ok "$FLOW" get-def --strip-root main.js 20 41

printf "\nTrace \`ParentFoo\` back to its def\n"
queries_in_file get-def "require.js"

printf "\nReact class and attribute expressions\n"
queries_in_file get-def "react.js"

printf "\nInheritance\n"
queries_in_file get-def "override.js"

printf "\nDirectly jump to required/imported modules (TODO #2)\n"
assert_ok "$FLOW" get-def --strip-root main.js 3 26
assert_ok "$FLOW" get-def --strip-root main.js 23 29
assert_ok "$FLOW" get-def --strip-root main.js 23 14
assert_ok "$FLOW" get-def --strip-root main.js 24 25
queries_in_file "get-def" "module_ref.js"

printf "\nRefinements\n"
queries_in_file "get-def" "refinements.js"

printf "\ndeclare var\n"
queries_in_file "get-def" "declare.js"

printf "\nShadowing \`require\`\n"
assert_ok "$FLOW" get-def --strip-root main.js 28 20
assert_ok "$FLOW" get-def --strip-root main.js 29 6
assert_ok "$FLOW" get-def --strip-root main.js 33 3

printf "\nChaining \`require\`s through multiple files\n"
assert_ok "$FLOW" get-def --strip-root main.js 33 10
assert_ok "$FLOW" get-def --strip-root main.js 33 17
