#!/bin/bash

# create_class.js
printf "create_calss.js:3:7 = "
assert_ok "$FLOW" type-at-pos create_class.js 4 7 --strip-root --pretty
printf "create_calss.js:19:7 = "
assert_ok "$FLOW" type-at-pos create_class.js 19 7 --strip-root --pretty
printf "create_calss.js:31:7 = "
assert_ok "$FLOW" type-at-pos create_class.js 31 7 --strip-root --pretty

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

printf "react_abstract_component.js:3:15 ="
assert_ok "$FLOW" type-at-pos react_abstract_component.js 3 15 --strip-root --pretty
