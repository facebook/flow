#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# import.js
printf "import.js:5:1 = "
assert_ok "$FLOW" type-at-pos import.js 5 1 --strip-root --pretty

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
printf "import-class-as-type.js:8:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 8 13 --strip-root --pretty
printf "import-class-as-type.js:9:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 9 13 --strip-root --pretty
printf "import-class-as-type.js:10:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 10 13 --strip-root --pretty
printf "import-class-as-type.js:11:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 11 13 --strip-root --pretty

# import-default.js
printf "import-default.js:15:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 15 13 --strip-root --pretty
printf "import-default.js:16:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 16 13 --strip-root --pretty
printf "import-default.js:17:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 17 13 --strip-root --pretty
printf "import-default.js:18:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 18 13 --strip-root --pretty
printf "import-default.js:19:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 19 13 --strip-root --pretty
printf "import-default.js:20:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 20 13 --strip-root --pretty
printf "import-default.js:21:13 = "
assert_ok "$FLOW" type-at-pos import-default.js 21 13 --strip-root --pretty

# import-typeof-class.js
printf "import-typeof-class.js:6:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 6 16 --strip-root --pretty --expand-json-output
printf "import-typeof-class.js:7:16 "
assert_ok "$FLOW" type-at-pos import-typeof-class.js 7 16 --strip-root --pretty --expand-json-output

# module-export-0.js
printf "module-export-0.js:7:13 = "
assert_ok "$FLOW" type-at-pos module-export-0.js 7 13 --strip-root --pretty

# module-export-1.js
printf "module-export-1.js:3:8 = "
assert_ok "$FLOW" type-at-pos module-export-1.js 3 8 --strip-root --pretty
printf "module-export-1.js:4:9 = "
assert_ok "$FLOW" type-at-pos module-export-1.js 4 9 --strip-root --pretty
printf "module-export-1.js:7:10 = "
assert_ok "$FLOW" type-at-pos module-export-1.js 7 10 --strip-root --pretty
printf "module-export-1.js:8:11 = "
assert_ok "$FLOW" type-at-pos module-export-1.js 8 11 --strip-root --pretty
printf "module-export-1.js:10:9 = "
assert_ok "$FLOW" type-at-pos module-export-1.js 10 9 --strip-root --pretty

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

# import-type.js
printf "import-type.js:3:20 = "
assert_ok "$FLOW" type-at-pos import-type.js 3 20 --strip-root --pretty
printf "import-type.js:7:7 = "
assert_ok "$FLOW" type-at-pos import-type.js 7 7 --strip-root --pretty --expand-json-output
