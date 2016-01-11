FLOW=$1
mkdir tmp
$FLOW status . --old-output-format
mv c.js tmp
$FLOW status . --old-output-format
mv b.js tmp
$FLOW status . --old-output-format
mv tmp/b.js .
$FLOW status . --old-output-format
mv tmp/c.js .
$FLOW status . --old-output-format
rmdir tmp
