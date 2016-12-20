FLOW=$1
mkdir tmp
cp *.js tmp/

# Rename A1.js to A2.js
mv A1.js A2.js
$FLOW force-recheck A1.js A2.js

# Ensure that A2.js @providesModule A2
cp tmp1A/A2.js A2.js
$FLOW force-recheck A2.js

# Update A3.js to require('A2')
cp tmp2A/A3.js A3.js
$FLOW force-recheck A3.js

$FLOW status

rm A2.js

mv tmp/*.js ./
rmdir tmp
