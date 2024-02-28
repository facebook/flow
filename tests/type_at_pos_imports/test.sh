#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

queries_in_file "type-at-pos" "exports-class-cjs.js"
queries_in_file "type-at-pos" "import-interface.js"
queries_in_file "type-at-pos" "import-class-as-type.js"
queries_in_file "type-at-pos" "import-class_of_poly_instance-es6.js"
queries_in_file "type-at-pos" "import-class_of_poly_instance-cjs.js"
queries_in_file "type-at-pos" "import-default.js"
queries_in_file "type-at-pos" "import-rec-export.js"
queries_in_file "type-at-pos" "import-star.js"
queries_in_file "type-at-pos" "import-typeof-class.js"
queries_in_file "type-at-pos" "module-export-0.js"
queries_in_file "type-at-pos" "module-export-1.js"
queries_in_file "type-at-pos" "module-import.js"
queries_in_file "type-at-pos" "require-class.js"
queries_in_file "type-at-pos" "import-type.js"
queries_in_file "type-at-pos" "rtype-main.js"
