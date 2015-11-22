FLOW=$1
mkdir tmp
$FLOW status . --old-output-format
mv dir/node_modules tmp/
$FLOW status . --old-output-format
mv tmp/node_modules dir/
$FLOW status . --old-output-format
rmdir tmp
