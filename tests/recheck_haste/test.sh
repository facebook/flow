#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp
cp ./*.js tmp/

printf "\nTest A:\n"
# Rename A1.js to A2.js
mv A1.js A2.js
assert_ok "$FLOW" force-recheck A1.js A2.js

# Ensure that A2.js @providesModule A2
cp tmp1A/A2.js A2.js
assert_ok "$FLOW" force-recheck A2.js

# Update A3.js to require('A2')
cp tmp2A/A3.js A3.js
assert_ok "$FLOW" force-recheck A3.js

assert_ok "$FLOW" status

# clean up
rm A2.js
cp tmp/A1.js A1.js
cp tmp/A3.js A3.js
# Done A

printf "\nTest B:\n"
# set up
cp -R dir1B tmp/

# mv dir1B/B2.js to dir2B/B2.js
mv dir1B dir2B
assert_ok "$FLOW" force-recheck dir1B/B2.js dir2B/B2.js

assert_ok "$FLOW" status

# clean up
mv dir2B dir1B
mv tmp/dir1B/*.js dir1B/
rmdir tmp/dir1B
# Done B

mv tmp/*.js ./
rmdir tmp
