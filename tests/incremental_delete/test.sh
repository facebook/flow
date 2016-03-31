FLOW=$1
mkdir tmp
$FLOW status . --old-output-format
mv c.js tmp
$FLOW force-recheck c.js
$FLOW status . --old-output-format
mv b.js tmp
$FLOW force-recheck b.js
$FLOW status . --old-output-format
mv tmp/b.js .
$FLOW force-recheck b.js
$FLOW status . --old-output-format
mv tmp/c.js .
$FLOW force-recheck c.js
$FLOW status . --old-output-format
rmdir tmp
