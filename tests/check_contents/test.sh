#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "syntax_error.js\n"
assert_errors \
  $FLOW check-contents --strip-root syntax_error.js < syntax_error.js

printf "syntax_error.js (no filename)\n"
assert_errors \
  $FLOW check-contents --strip-root < syntax_error.js

printf "\n\nnot_flow.js\n\n"
assert_errors \
  $FLOW check-contents --strip-root not_flow.js < not_flow.js

printf "\n\nnot_flow.js with --all\n\n"
assert_errors \
  $FLOW check-contents --strip-root --all not_flow.js < not_flow.js

printf "\n\nnot_flow.js with --respect-pragma\n\n"
assert_ok \
  $FLOW check-contents --strip-root --respect-pragma not_flow.js < not_flow.js

printf "\n\nnot_flow.js with --respect-pragma and --all\n\n"
assert_errors \
  $FLOW check-contents --strip-root --respect-pragma --all not_flow.js < not_flow.js

printf "\n\nunsaved_foo.js depends on bar.js which is still checked against foo.js\n\n"
assert_ok \
  $FLOW check-contents --strip-root foo.js < unsaved_foo.js

printf "\n\nbad_exports.js\n\n"
assert_errors \
  $FLOW check-contents --strip-root bad_exports.js < bad_exports.js
