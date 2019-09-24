#!/bin/bash
. ../fs.sh
printf "\nShould have all expected errors\n"
assert_errors "$FLOW" status .
printf "\nShould fix incrementalRoot\n"
remove subdir/custom_resolve_dir/incrementalRoot/index.js
assert_errors "$FLOW" status .
printf "\nShould fix incrementalNodeModules\n"
remove custom_resolve_dir/incrementalNodeModules/index.js
assert_errors "$FLOW" status .
