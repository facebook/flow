#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# exports-class-cjs.js
printf "exports-class-cjs.js:3:24 = "
assert_ok "$FLOW" type-at-pos exports-class-cjs.js 3 24 --strip-root --pretty
printf "exports-class-cjs.js:5:25 = "
assert_ok "$FLOW" type-at-pos exports-class-cjs.js 5 25 --strip-root --pretty

# import-interface.js
printf "import-interface.js:3:15 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 3 15 --strip-root --pretty --expand-json-output
printf "import-interface.js:3:25 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 3 25 --strip-root --pretty --expand-json-output
printf "import-interface.js:5:15 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 5 15 --strip-root --pretty --expand-json-output
printf "import-interface.js:5:27 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 5 27 --strip-root --pretty --expand-json-output
printf "import-interface.js:5:38 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 5 38 --strip-root --pretty --expand-json-output
printf "import-interface.js:5:48 (--expand-json-output) = "
assert_ok "$FLOW" type-at-pos import-interface.js 5 48 --strip-root --pretty --expand-json-output

# import-class-as-type.js
printf "import-class-as-type.js:8:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 8 13 --strip-root --pretty
printf "import-class-as-type.js:9:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 9 13 --strip-root --pretty
printf "import-class-as-type.js:10:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 10 13 --strip-root --pretty
printf "import-class-as-type.js:11:13 = "
assert_ok "$FLOW" type-at-pos import-class-as-type.js 11 13 --strip-root --pretty

# import-class_of_poly_instance-es6.js
printf "import-class_of_poly_instance-es6.js:4:8 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-es6.js 4 8 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-es6.js:5:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-es6.js 5 13 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-es6.js:7:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-es6.js 7 13 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-es6.js:8:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-es6.js 8 13 --strip-root --pretty --expand-json-output

# import-class_of_poly_instance-cjs.js
printf "import-class_of_poly_instance-cjs.js:3:8 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-cjs.js 3 8 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-cjs.js:4:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-cjs.js 4 13 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-cjs.js:6:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-cjs.js 6 13 --strip-root --pretty --expand-json-output
printf "import-class_of_poly_instance-cjs.js:7:13 = "
assert_ok "$FLOW" type-at-pos import-class_of_poly_instance-cjs.js 7 13 --strip-root --pretty --expand-json-output

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
printf "import-default.js:26:7 = "
assert_ok "$FLOW" type-at-pos import-default.js 26 7 --strip-root --pretty --expand-json-output

# import-rec-export.js
printf "import-rec-export.js:3:10 = "
assert_ok "$FLOW" type-at-pos import-rec-export.js 3 10 --strip-root --pretty

# import-star.js
printf "import-star.js:5:7 = "
assert_ok "$FLOW" type-at-pos import-star.js 5 7 --strip-root --pretty
printf "import-star.js:5:31 = "
assert_ok "$FLOW" type-at-pos import-star.js 5 31 --strip-root --pretty

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

# import-type.js
printf "import-type.js:3:20 = "
assert_ok "$FLOW" type-at-pos import-type.js 3 20 --strip-root --pretty
printf "import-type.js:7:7 = "
assert_ok "$FLOW" type-at-pos import-type.js 7 7 --strip-root --pretty --expand-json-output
