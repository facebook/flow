#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "Baseline: the libdef present at startup is global\n"
assert_ok "$FLOW" status --strip-root

printf "\nAdd a libdef while the server is running\n"
cp extra.js.ignored libs/extra.js
assert_ok "$FLOW" force-recheck --no-auto-start libs/extra.js

cat > b.js <<'EOF'
const y: number = MyNewGlobal;
const bad: string = MyNewGlobal;

module.exports = {y, bad};
EOF
cat > import-new.js <<'EOF'
import {MyNewGlobal as Imported} from './libs/extra';

module.exports = {Imported};
EOF
assert_ok "$FLOW" force-recheck --no-auto-start b.js import-new.js

printf "\nThe new declaration is global and the libdef resolves as a non-module\n"
assert_errors "$FLOW" status --strip-root

# `flow ls --explain` answers the same question from the CLI, against the
# `[libs]` config rather than a running server, so it has to agree.
printf "\nflow ls --explain classifies both libdefs\n"
assert_ok "$FLOW" ls --explain --strip-root --all
