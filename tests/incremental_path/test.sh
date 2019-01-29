#!/bin/bash
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
