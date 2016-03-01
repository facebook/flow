FLOW=$1

mv bar.js _bar.js
sleep 1 # TODO: add --wait or something better than this
$FLOW status --old-output-format .
mv _bar.js bar.js
