
FLOW=$1

printf "\nInitial status:\n"
$FLOW status --no-auto-start --old-output-format .

printf "\nCreate data.json:\n"
cp data.json1 data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start . --old-output-format

printf "\nModify data.json:\n"
cp data.json2 data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start . --old-output-format

printf "\nDelete data.json:\n"
rm data.json
$FLOW force-recheck --no-auto-start test.js
$FLOW status --no-auto-start --old-output-format .

printf "\nDone!\n"
