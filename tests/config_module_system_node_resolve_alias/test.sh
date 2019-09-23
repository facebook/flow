#!/bin/bash
. ../fs.sh
mkdir -p tmp/subdir/custom_resolve_dir tmp/custom_resolve_dir
printf "\nShould have all 3 expected errors\n"
assert_errors "$FLOW" status .
printf "\nShould fix the first error\n"
move subdir/custom_resolve_dir/testproj2 tmp/subdir/custom_resolve_dir/testproj2
assert_errors "$FLOW" status .
printf "\nShould fix the second error\n"
move custom_resolve_dir/testproj2 tmp/custom_resolve_dir/testproj2
assert_errors "$FLOW" status .
printf "\nShould fix the third error\n"
move custom_resolve_dir/testproj tmp/custom_resolve_dir/testproj
mv tmp/custom_resolve_dir/testproj custom_resolve_dir/testproj
mv tmp/custom_resolve_dir/testproj2 custom_resolve_dir/testproj2
mv tmp/subdir/custom_resolve_dir subdir/custom_resolve_dir
rm -rf tmp
