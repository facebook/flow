FLOW=$1
mkdir tmp
cp root.js tmp/
$FLOW status .
cp tmp1/root.js ./
$FLOW force-recheck root.js
$FLOW status .
mv tmp/root.js ./
rmdir tmp
