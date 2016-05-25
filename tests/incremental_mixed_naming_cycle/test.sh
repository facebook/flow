FLOW=$1
mkdir tmp
cp root.js tmp/
$FLOW status . --old-output-format
cp tmp1/root.js ./
$FLOW force-recheck root.js
$FLOW status . --old-output-format
mv tmp/root.js ./
rmdir tmp
