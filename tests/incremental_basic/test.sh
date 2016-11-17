FLOW=$1
mkdir tmp
cp *.js tmp/
$FLOW status .
cp tmp1/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status .
cp tmp2/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status .
cp tmp3/*.js ./
$FLOW force-recheck *.js # overapproximation
$FLOW status .
mv tmp/*.js ./
rmdir tmp
