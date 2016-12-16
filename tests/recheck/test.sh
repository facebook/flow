FLOW=$1
mkdir tmp
cp *.js tmp/

printf "\nTest a:\n"
$FLOW status .
cp tmp1a/a1.js ./
$FLOW force-recheck a1.js
$FLOW status .
cp tmp2a/a1.js ./
$FLOW force-recheck a1.js
$FLOW status .
rm a*.js

printf "\nTest b:\n"
cp tmp1b/b1.js ./
$FLOW force-recheck b1.js
$FLOW status .
cp tmp2b/b0.js ./
$FLOW force-recheck b0.js
$FLOW status .
rm b*.js

printf "\nTest c:\n"
cp tmp1c/c2.js ./
$FLOW force-recheck c2.js
$FLOW status .
cp tmp2c/c1.js ./
$FLOW force-recheck c1.js
$FLOW status .
rm c*.js

printf "\nTest d:\n"
cp tmp1d/d1.js ./
$FLOW force-recheck d1.js
$FLOW status .
rm d*.js

printf "\nTest e:\n"
cp tmp1e/e2.js ./
$FLOW force-recheck e2.js
$FLOW status .
cp tmp2e/e1.js ./
$FLOW force-recheck e1.js
$FLOW status .
cp tmp3e/e1.js tmp3e/e2.js ./
$FLOW force-recheck e1.js e2.js
$FLOW status .
rm e*.js

printf "\nTest f:\n"
cp tmp1f/f1.js ./
$FLOW force-recheck f1.js
$FLOW status .
cp tmp2f/f1.js ./
$FLOW force-recheck f1.js
$FLOW status .
cp tmp3f/f1.js ./
$FLOW force-recheck f1.js
$FLOW status .
cp tmp4f/f1.js ./
$FLOW force-recheck f1.js
$FLOW status .
rm f*.js

printf "\nTest g:\n"
cp tmp1g/g1.js ./
$FLOW force-recheck g1.js
$FLOW status .
rm g*.js

mv tmp/*.js ./
rmdir tmp
