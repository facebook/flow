FLOW=$1
mkdir tmp
cp *.js tmp/
$FLOW status . --old-output-format
cp tmp1/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status . --old-output-format
cp tmp2/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status . --old-output-format
cp tmp3/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status . --old-output-format
mv tmp/*.js ./
rmdir tmp
