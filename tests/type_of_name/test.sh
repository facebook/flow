#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Basic types
printf "=== Basic types ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js myString --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js myNumber --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js myBoolean --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js myArray --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js myObject --strip-root

# Functions
printf "\n=== Functions ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js myFunction --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js arrowFunc --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js param --strip-root

# Classes
printf "\n=== Classes ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js MyClass --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js instance --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js methodName --strip-root

# Type aliases
printf "\n=== Type aliases ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js MyType --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js typeVar --strip-root

# Destructuring
printf "\n=== Destructuring ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js destructuredX --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js destructuredY --strip-root

# Error cases
printf "\n=== Error cases (muted output) ===\n"
assert_exit "$EXIT_USAGE" "$FLOW" type-of-name-experimental basic.js > /dev/null
assert_errors "$FLOW" type-of-name-experimental basic.js nonexistent # prints "not found"

# React components
printf "\n=== React Components ===\n"
assert_ok "$FLOW" type-of-name-experimental react.js BasicComponent --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js OptionalProps --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js GenericComponent --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js InexactRest --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js IndexedRest --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js NamedProps --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js UserCard --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js DataDisplay --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js ProductItem --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js UserProfile --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js NotificationWithDocblock --strip-root --unexpand-component-props

printf "\n=== React Components (--hide-references) ===\n"
assert_ok "$FLOW" type-of-name-experimental react.js UserCard --strip-root --hide-references --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js DataDisplay --strip-root --hide-references --unexpand-component-props

printf "\n=== React Components (--expand-component-props) ===\n"
assert_ok "$FLOW" type-of-name-experimental react.js UserCard --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js DataDisplay --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js ProductItem --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental react.js UserProfile --strip-root --expand-component-props

# Check indexer with requests from empty file
printf "\n=== React Components in empty file ===\n"
assert_ok "$FLOW" type-of-name-experimental empty_file.js BasicComponent --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js OptionalProps --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js GenericComponent --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js InexactRest --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js IndexedRest --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js NamedProps --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js UserCard --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js DataDisplay --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js ProductItem --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js UserProfile --strip-root --unexpand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js NotificationWithDocblock --strip-root --unexpand-component-props

assert_ok "$FLOW" type-of-name-experimental empty_file.js UserCard --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js DataDisplay --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js ProductItem --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js UserProfile --strip-root --expand-component-props
assert_ok "$FLOW" type-of-name-experimental empty_file.js UserCard --strip-root --exact-match-only --unexpand-component-props # returns result from react.js

assert_errors "$FLOW" type-of-name-experimental empty_file.js NO_UserProfile --strip-root --exact-match-only

# Check builtins
assert_ok "$FLOW" type-of-name-experimental empty_file.js Array --strip-root

# Enums
printf "\n=== Enums ===\n"
assert_ok "$FLOW" type-of-name-experimental basic.js MyStringEnum --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js MyNumberEnum --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js MyBooleanEnum --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js enumValue --strip-root
assert_ok "$FLOW" type-of-name-experimental basic.js LargeEnum --strip-root
