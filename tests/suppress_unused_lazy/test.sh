#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

# OVERVIEW
#
# a.js and b.js form a cycle, so that b.js is both a dependency and
# a dependent of a.js. everything else are dependents of b.js.
#
# If we check dependencies of a.js, we'll merge and check b.js. Then
# when we check dependents of a.js, we'll merge and check the other test files,
# but hit a recheck optimization because the dependency (b.js) is already checked
# and unchanged.
#
# Suppressions are currently discovered during merge, so if we skip
# checking we never discover the errors that are being suppressed and could end
# up warning about an unused suppression. On the other hand, signature
# verification errors occur during parsing and those also can be suppressed.
# They should only appear on actually-checked files (not "checked" files that
# get skipped due to recheck opts).

# check-contents a.js, which does a dependencies-only check. this merges
# and checks a.js and b.js
printf "Status after check-contents a.js:\n"
assert_ok "$FLOW" check-contents a.js < a.js

printf "Status including warnings:\n"
assert_ok "$FLOW" status --include-warnings

# recheck a.js and dependents
assert_ok "$FLOW" force-recheck --focus a.js

printf "\nStatus after focusing a.js:\n"
# TODO: this should be ok!
assert_errors "$FLOW" status --include-warnings

# check-contents doesn't hit recheck opts
printf "Status after check-contents type_error.js:\n"
assert_errors "$FLOW" check-contents type_error.js < type_error.js

# should not notice the error in type_error because it's not focused
printf "\nStatus including warnings:\n"
# TODO: this should be ok!
assert_errors "$FLOW" status --include-warnings

# focus dependents
assert_ok "$FLOW" force-recheck --focus \
  type_error.js \
  type_error_suppressed.js \
  invalid_export.js \
  invalid_export_suppressed.js \
  signature_verification_error.js \
  signature_verification_error_suppressed.js

# should notice the errors because they're focused
printf "\nStatus after focusing dependents:\n"
assert_errors "$FLOW" status --include-warnings
