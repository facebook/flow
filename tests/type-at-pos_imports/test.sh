#!/bin/bash

# exports.js
printf "exports.js:3:24 = "
assert_ok "$FLOW" type-at-pos exports.js 3 24 --strip-root --pretty
printf "exports.js:5:25 = "
assert_ok "$FLOW" type-at-pos exports.js 5 25 --strip-root --pretty

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

# import-class-as-type.js
printf "import-class-as-type.js:6:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 6 13 --strip-root --pretty

# import-default.js
printf "import-default.js:3:16 = "
assert_ok "$FLOW" type-at-pos import-default.js 3 16 --strip-root --pretty

# import-typeof-class.js
printf "import-typeof-class.js:6:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 6 16 --strip-root --pretty --expand-json-output
printf "import-typeof-class.js:7:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 7 16 --strip-root --pretty --expand-json-output

# module-export.js
printf "module-export.js:7:13 = "
assert_ok "$FLOW" type-at-pos module-export.js 7 13 --strip-root --pretty

# module-import.js
printf "module-import.js:3:7 = "
assert_ok "$FLOW" type-at-pos module-import.js 3 7 --strip-root --pretty

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
