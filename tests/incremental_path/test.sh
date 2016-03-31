FLOW=$1
mkdir tmp
$FLOW status . --old-output-format
mv dir/node_modules tmp/
$FLOW force-recheck dir/node_modules/*.js
$FLOW status . --old-output-format
mv tmp/node_modules dir/
$FLOW force-recheck dir/node_modules/*.js
$FLOW status . --old-output-format
rmdir tmp
