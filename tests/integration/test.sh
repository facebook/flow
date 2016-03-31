FLOW=$1

mv bar.js _bar.js
$FLOW force-recheck bar.js _bar.js
$FLOW status --old-output-format .
mv _bar.js bar.js
