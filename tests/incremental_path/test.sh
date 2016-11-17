FLOW=$1
mkdir tmp
$FLOW status .
mv dir/node_modules tmp/
$FLOW force-recheck dir/node_modules/*.js
$FLOW status .
mv tmp/node_modules dir/
$FLOW force-recheck dir/node_modules/*.js
$FLOW status .
rmdir tmp
