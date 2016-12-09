FLOW=$1
mkdir tmp
cp *.js tmp/
$FLOW status .
cp tmp1a/a1.js ./
$FLOW force-recheck a1.js
$FLOW status .
cp tmp2a/a1.js ./
$FLOW force-recheck a1.js
$FLOW status .
cp tmp1b/b1.js ./
$FLOW force-recheck b1.js
$FLOW status .
cp tmp2b/b0.js ./
$FLOW force-recheck b0.js
$FLOW status .
cp tmp1c/c2.js ./
$FLOW force-recheck c2.js
$FLOW status .
cp tmp2c/c1.js ./
$FLOW force-recheck c1.js
$FLOW status .
cp tmp1d/d1.js ./
$FLOW force-recheck d1.js
$FLOW status .
mv tmp/*.js ./
rmdir tmp
