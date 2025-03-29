#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# shellcheck disable=SC2094

assert_ok "$FLOW" status --no-auto-start .

printf "\nPrepend whitespace\n"
echo "

// \$FlowFixMe[libdef-override]
type Foo = string;
type Foo = string;" > flow-typed/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start flow-typed/lib.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nRemove suppression\n"
echo "type Foo = string;
type Foo = string;" > flow-typed/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start flow-typed/lib.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nFix libdef override errors\n"
echo "type Foo = string;" > flow-typed/lib.js
assert_ok "$FLOW" force-recheck --no-auto-start flow-typed/lib.js
assert_ok "$FLOW" status --no-auto-start .
