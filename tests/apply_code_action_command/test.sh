#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
cp ./*.js tmp/

start_flow tmp

echo '> apply-code-action '\''source.addMissingImports'\'' tmp/a.js'
assert_ok "$FLOW" apply-code-action 'source.addMissingImports' tmp/a.js
assert_ok "$FLOW" apply-code-action 'source.addMissingImports' --in-place tmp/a.js
echo '> Confirm no errors'
assert_ok "$FLOW" focus-check tmp/a.js
echo '> apply-code-action '\''source.addMissingImports'\'' tmp/multi.js'
assert_ok "$FLOW" apply-code-action 'source.addMissingImports' tmp/multi.js
assert_ok "$FLOW" apply-code-action 'source.addMissingImports' --in-place tmp/multi.js
echo '> Confirm no errors'
assert_ok "$FLOW" focus-check tmp/multi.js
echo '> apply-code-action suggestImports tmp/suggest_imports.js'
"$FLOW" apply-code-action suggestImports --pretty tmp/suggest_imports.js | sed -e 's/file:.*:/file:tmp\/suggest_imports.js": /'

echo '> apply-code-action '\''experimental.quickfix'\'' tmp/test-quickfixes.js'
assert_ok "$FLOW" apply-code-action 'experimental.quickfix' tmp/test-quickfixes.js
echo '> apply-code-action '\''experimental.quickfix'\'' tmp/test-quickfixes.js --include-best-effort-fix'
assert_ok "$FLOW" apply-code-action 'experimental.quickfix' tmp/test-quickfixes.js --include-best-effort-fix
