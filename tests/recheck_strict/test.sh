#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

. ../fs.sh

"$FLOW" stop
"$FLOW" start --lazy-mode none

cp ./commit_a/*.js .

printf "Run flow on commit A\n"
assert_ok "$FLOW" status .

cp ./commit_b/*.js .

printf "\nRun flow on commit B\n"
assert_ok "$FLOW" status .

rm *.js
