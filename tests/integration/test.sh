FLOW=$1

mv bar.js _bar.js
$FLOW status --wait-for-recheck --old-output-format .
mv _bar.js bar.js
