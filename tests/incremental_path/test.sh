#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

. ../fs.sh
mkdir -p tmp/node_modules
printf "\nShould resolve to dir/node_modules/b.js which is number\n"
assert_errors "$FLOW" status .
printf "\nShould resolve to node_modules/b.js which is a string\n"
move dir/node_modules/b.js tmp/node_modules/b.js
assert_errors "$FLOW" status .
printf "\nShould resolve to dir/node_modules/b.js which is number\n"
move tmp/node_modules/b.js dir/node_modules/b.js
assert_errors "$FLOW" status .
rm -r tmp
